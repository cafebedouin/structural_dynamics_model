% ============================================================================
% CONSTRAINT STORY: competition_timeline_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competition_timeline_pressure, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competition_timeline_pressure
 *   human_readable: Competition Timeline Pressure in Age Reversal Trials
 *   domain: biomedical_research/regulatory_science/technology_governance
 *
 * SUMMARY:
 *   The XPrize Age Reversal Competition requires teams to demonstrate 10
 *   years of biological age reversal in 1 year of treatment, creating
 *   structural pressure to compress safety validation timelines that would
 *   normally span 5-10 years in traditional drug development. This constraint
 *   operates at the intersection of innovation incentives, regulatory
 *   science, and participant protection. The competition solves a real
 *   coordination problem — age reversal research was chronically underfunded
 *   and lacked clear success metrics — but the timeline requirement
 *   structurally incentivizes trial designs that transfer long-term safety
 *   risk from research teams and organizers to trial participants. The
 *   constraint exhibits tangled rope characteristics: genuine coordination
 *   function (mobilizing capital and talent toward transformative research)
 *   coexists with asymmetric extraction (compressed safety monitoring that
 *   cannot detect delayed toxicities within competition window). Theater
 *   ratio (0.48) reflects moderate performative content: safety monitoring
 *   protocols are maintained but their intervals are compressed to fit
 *   competition deadlines, creating appearance of validation without the
 *   temporal depth required for long-term toxicity detection. Suppression
 *   (0.67) reflects barriers to alternative pathways: once enrolled,
 *   participants face high exit costs (loss of experimental therapy access),
 *   and research teams face career and funding penalties for withdrawing from
 *   competition.
 *
 * KEY AGENTS:
 *   - Trial Participants: Primary victims (powerless/trapped) — bear health risk of compressed safety validation; cannot exit without losing therapy access; information asymmetry about timeline-driven protocol compression
 *   - Competition Organizers: Primary beneficiaries (institutional/arbitrage) — capture reputational benefit and platform visibility; externalize safety risk; retain exit options to modify rules or withdraw
 *   - Participating Research Teams: Mixed position (moderate/constrained) — benefit from competition funding and visibility; bear ethical burden of compressed protocols; constrained by competition rules and career incentives
 *   - Regulatory Agencies: Institutional actors (institutional/constrained) — dual mandate creates tension between protecting safety and enabling innovation; constrained by political pressure to appear innovation-friendly; bear institutional risk if long-term harms emerge
 *   - Long-term Safety Validation: Abstract victim (powerless/trapped) — epistemic commons that cannot advocate for itself; compressed timelines prevent detection of delayed toxicities that may only emerge after competition window closes
 *   - Bioethics Coalition: Organized advocates (organized/mobile) — building alternative frameworks (adaptive trials, rolling reviews, participant registries) with sunset logic; mobile exit to other governance mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competition_timeline_pressure, 0.58).
domain_priors:suppression_score(competition_timeline_pressure, 0.67).
domain_priors:theater_ratio(competition_timeline_pressure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competition_timeline_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(competition_timeline_pressure, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(competition_timeline_pressure, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competition_timeline_pressure, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competition_timeline_pressure, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competition_timeline_pressure, tangled_rope).
narrative_ontology:human_readable(competition_timeline_pressure, "Competition Timeline Pressure in Age Reversal Trials").
narrative_ontology:topic_domain(competition_timeline_pressure, "biomedical_research/regulatory_science/technology_governance").

domain_priors:requires_active_enforcement(competition_timeline_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competition_timeline_pressure, competition_organizers).
narrative_ontology:constraint_beneficiary(competition_timeline_pressure, participating_research_teams).
narrative_ontology:constraint_victim(competition_timeline_pressure, trial_participants).
narrative_ontology:constraint_victim(competition_timeline_pressure, long_term_safety_validation).
narrative_ontology:constraint_vindicates(competition_timeline_pressure, innovation_acceleration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enrolled in accelerated age reversal trial with compressed safety monitoring intervals. Cannot exit without losing access to experimental therapy. Bears health risk of unknown long-term effects while competition deadline drives protocol compression. Consent process may not adequately convey timeline-driven safety validation compression.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, trial_participants, payer,
    powerless, biographical, trapped, local).

% Set competition rules including 10-year-improvement-in-1-year timeline requirement. Capture reputational benefit and platform visibility from competition. Externalize safety risk to trial participants and research teams. Retain exit options: can modify rules, extend deadlines, or withdraw if reputational cost exceeds benefit.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, competition_organizers, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competition_timeline_pressure, competition_organizers, beneficiary).

% Benefit from competition funding and visibility that would not otherwise be available for age reversal research. Face ethical burden of compressed safety protocols. Constrained by competition rules and career incentives: withdrawing means losing funding and competitive position. Must balance innovation opportunity against participant safety.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, participating_research_teams, beneficiary,
    moderate, biographical, constrained, national).

% Dual mandate: protect participant safety while enabling potentially transformative research. Face political pressure to approve compressed protocols under expedited pathways. Bear institutional risk if long-term harms emerge after competition window closes. Cannot fully block competition without political cost; cannot fully approve without adequate safety data.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Organized coalition building alternative frameworks: adaptive trial designs, rolling safety reviews, participant registries. See timeline pressure as temporary coordination failure with sunset logic. Mobile exit: can shift advocacy to other governance mechanisms if competition model proves unreformable. Actively resist timeline compression through public commentary and regulatory engagement.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, bioethics_advocates, observer,
    organized, generational, mobile, global).

% Abstract epistemic commons. Compressed timelines prevent detection of delayed toxicities that may only emerge 5-10 years post-treatment. Cannot advocate for itself. Bears full cost of timeline-driven protocol compression as field-level knowledge gap.
narrative_ontology:constraint_stakeholder(competition_timeline_pressure, long_term_safety_validation, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(competition_timeline_pressure, long_term_safety_validation).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competition_timeline_pressure, competition_organizers).
narrative_ontology:fixing_cost_class(competition_timeline_pressure, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The competition solves a real coordination problem: age reversal research was chronically underfunded and lacked clear success metrics. The XPrize creates both funding mobilization and a concrete target (10 years reversal in 1 year treatment), enabling research that would not otherwise occur.
% TRANSFER_FUNCTION: The arrangement transfers health risk from competition organizers and research teams to trial participants. Organizers and teams capture immediate benefits (reputational visibility, funding, career advancement) while participants bear long-term safety risk that cannot be detected within the 1-year competition window. Risk transfer is enabled by compressed safety monitoring intervals and information asymmetry in consent process.
% ABSENT_VOICES: Long-term safety validation has no advocate in the competition design process. Trial participants are present but face information asymmetry about timeline-driven risk compression. Future patients who may be harmed by therapies approved based on insufficient long-term data are not represented. Regulatory scientists concerned about compressed validation timelines are present but constrained by political pressure to enable innovation.
% DISAPPEARANCE_RATIONALE: If the competition disappeared, age reversal research would revert to traditional funding and timeline structures. Research teams would lose access to competition resources and would need to design trials with standard 5-10 year safety validation windows. Trial participants would face different risk-benefit calculations without timeline pressure. Regulatory agencies would apply standard approval criteria without expedited pathway pressure. The coordination function (research mobilization) would be lost, but so would the extraction mechanism (compressed safety validation).
% FOUNDING_PROBLEM: Age reversal research was chronically underfunded and lacked clear success metrics or coordination mechanisms to mobilize capital and talent. The field needed both resources and concrete targets to make progress on transformative but high-risk research.
% FOUNDING_PROBLEM_CORROBORATION: The funding gap is corroborated by NIH budget data showing minimal allocation to age reversal research prior to private competition launch. The lack of clear success metrics is corroborated by geroscience researchers outside the competition (e.g., publications in Nature Aging, Science) noting the field's historical difficulty in defining measurable endpoints. The coordination problem is corroborated by venture capital analysis showing limited investment in age reversal prior to high-profile competitions creating market signals.
narrative_ontology:disappearance_verdict(competition_timeline_pressure, world_rearranges).
narrative_ontology:founding_problem_status(competition_timeline_pressure, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRIAL PARTICIPANT (SNARE) — Enrolled in accelerated trial with compressed safety monitoring. Cannot exit once enrolled without losing access to experimental therapy. Bears full risk of unknown long-term effects while competition deadline drives protocol compression. Maximum extraction: health risk transferred to participant while timeline benefit accrues to organizers and research teams.
constraint_indexing:constraint_classification(competition_timeline_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRINCIPAL INVESTIGATOR (TANGLED ROPE) — Faces genuine coordination problem: XPrize creates funding and visibility for age reversal research that would otherwise lack resources. But the 1-year timeline compresses safety validation that should take 5-10 years. PI benefits from competition resources while bearing ethical burden of compressed protocols. Mixed extraction: coordination function (research funding) coexists with asymmetric risk transfer to participants.
constraint_indexing:constraint_classification(competition_timeline_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETITION ORGANIZER (ROPE) — Experiences constraint as pure coordination: the timeline creates urgency that mobilizes capital and talent toward age reversal research. Organizer captures reputational benefit and platform visibility while externalizing safety risk to trial participants. Net beneficiary with exit options: can modify rules, extend deadlines, or withdraw if reputational cost exceeds benefit.
constraint_indexing:constraint_classification(competition_timeline_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Constrained by dual mandate: must protect participant safety while not blocking potentially transformative research. Competition timeline creates pressure to approve compressed protocols under expedited pathways. Agency benefits from appearing innovation-friendly while bearing institutional risk if long-term harms emerge. Constrained exit: cannot fully block competition without political cost, cannot fully approve without safety data.
constraint_indexing:constraint_classification(competition_timeline_pressure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BIOETHICS COALITION (SCAFFOLD) — Organized advocates see the timeline pressure as a temporary coordination failure with a sunset: as the first competition cycle completes and long-term data emerges, future competitions will incorporate learned safety requirements. Coalition is building alternative frameworks (adaptive trial designs, rolling safety reviews, participant registries) that preserve innovation incentives while extending monitoring windows. Mobile exit: can shift advocacy to other governance mechanisms if competition model proves unreformable.
constraint_indexing:constraint_classification(competition_timeline_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The competition timeline solves a real coordination problem: age reversal research was chronically underfunded and lacked clear success metrics. The XPrize creates both. But the 10-year-improvement-in-1-year requirement structurally incentivizes skipping long-term safety validation that cannot be compressed without risk transfer. Genuine coordination function coexists with asymmetric extraction. The constraint requires active enforcement: competition rules must be maintained, regulatory expedited pathways must be invoked, and participant consent must be obtained under compressed information conditions.
constraint_indexing:constraint_classification(competition_timeline_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competition_timeline_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competition_timeline_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competition_timeline_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competition_timeline_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competition_timeline_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The competition timeline creates asymmetric risk transfer: organizers and research teams capture immediate benefits (funding, visibility, career advancement) while participants bear long-term health risks that cannot be detected within the 1-year competition window. The extraction is not maximal because some genuine coordination benefit exists (research that would not otherwise be funded), but the timeline compression structurally prevents adequate safety validation. Suppression (0.67): High. Multiple barriers constrain alternatives: participants face loss of experimental therapy access if they exit; research teams face career and funding penalties for withdrawing; regulatory agencies face political cost for blocking innovation. The suppression has increased over the interval as competition deadlines approach and sunk costs accumulate. Theater ratio (0.48): Moderate. Safety monitoring protocols are maintained but compressed to fit competition timelines. Interim reviews occur but at intervals too short to detect delayed toxicities. Consent processes are followed but may not adequately convey timeline-driven risk compression. The theater is real but not dominant — some functional safety monitoring occurs, just insufficient for long-term validation. Accessibility collapse (0.42): Moderate. Alternative pathways exist (traditional drug development, non-competition research funding) but are substantially less attractive due to resource and visibility asymmetry. The competition does not completely collapse alternatives but makes them significantly harder to access. Resistance (0.61): Substantial. Bioethics advocates, some regulatory officials, and participant advocacy groups actively resist timeline compression. The resistance is real and organized but has not prevented competition launch or protocol approval.
 *
 * PERSPECTIVAL GAP:
 *   The competition organizer sees pure coordination (Rope) — the timeline creates urgency that mobilizes resources toward transformative research. The trial participant sees pure extraction (Snare) — health risk is transferred while benefits accrue to organizers and research teams, with no real exit option once enrolled. The principal investigator sees tangled rope — genuine coordination benefit (research funding) coexists with ethical burden of compressed safety validation. The regulatory agency also sees tangled rope — must enable innovation while protecting participants, with constrained exit options. The bioethics coalition sees scaffold — the timeline pressure is temporary, and alternative frameworks are being built with learned safety requirements. The analytical observer sees tangled rope at the civilizational scale — the competition solves a real coordination problem but structurally incentivizes risk transfer that requires active enforcement to maintain. The perspectival gap reveals that 'innovation acceleration' appears as coordination or extraction depending on who bears the risk and who captures the benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Trial participants are full victims with trapped exit options, producing high directionality toward target end (d approaching 1.0). They bear health risk while benefits accrue elsewhere. Competition organizers are primary beneficiaries with arbitrage exit options, producing low directionality toward beneficiary end (d approaching 0.0). They capture reputational and platform benefits while externalizing risk. Principal investigators occupy mixed position: benefit from competition resources but bear ethical burden of compressed protocols; constrained exit options produce moderate directionality (d around 0.4-0.5). Regulatory agencies are institutional actors with constrained exit, experiencing moderate extraction (d around 0.5) — they must balance safety mandate against innovation pressure. The bioethics coalition has mobile exit options and organized power, producing low effective extraction (d around 0.3) — they can shift advocacy to alternative governance mechanisms. Long-term safety validation is an abstract victim with no exit options, producing maximum directionality (d = 1.0) — the epistemic commons cannot advocate for itself and bears full cost of compressed timelines.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope classification captures the structural reality: genuine coordination function (mobilizing underfunded research) coexists with asymmetric extraction (compressed safety validation that transfers risk to participants). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) — it is both simultaneously. The competition organizer's rope perspective is their genuine experience but incomplete: they see the coordination benefit while externalizing the extraction cost. The trial participant's snare perspective is also genuine but incomplete: they bear the extraction without seeing the coordination function that benefits the field. The analytical observer's tangled rope classification integrates both perspectives: the timeline requirement solves a real problem (research funding and clear success metrics) while creating a real harm (compressed safety validation). The mandatrophy is resolved not by choosing one type but by recognizing that the constraint's structure contains both coordination and extraction, with the balance depending on the observer's position in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptive_trial_sufficiency,
    'Can adaptive trial designs with rolling safety reviews provide equivalent long-term safety data within compressed competition timelines?',
    'Comparison of adverse event detection rates and latency between traditional fixed-protocol trials and adaptive designs; analysis of whether interim monitoring catches delayed toxicities that would emerge in extended follow-up',
    'If adaptive designs are sufficient: timeline pressure is coordination problem (Rope from more perspectives). If insufficient: timeline pressure is extraction mechanism transferring undetectable risk to participants (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_trial_sufficiency, empirical, 'Whether adaptive trial designs provide equivalent safety data under timeline compression').

omega_variable(
    competition_learning_curve,
    'Will subsequent competition cycles incorporate safety requirements learned from first-cycle long-term follow-up, or will timeline pressure persist as structural feature?',
    'Longitudinal analysis of competition rule evolution; comparison of safety monitoring requirements across multiple prize cycles; assessment of whether organizers extend timelines or maintain compression despite emerging safety signals',
    'If learning occurs: scaffold perspective confirmed — sunset is real. If timeline pressure persists: the compression is structural extraction, not temporary coordination failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competition_learning_curve, empirical, 'Whether competition structure learns from safety outcomes or maintains timeline pressure').

omega_variable(
    participant_information_asymmetry,
    'Do trial participants understand that competition timeline compresses safety validation relative to standard drug development, and does informed consent adequately convey this risk?',
    'Analysis of consent documents for timeline-risk disclosure; participant comprehension studies; comparison of participant risk perception vs actual protocol compression magnitude',
    'If participants understand compression: consent is valid and extraction is reduced. If participants do not understand: information asymmetry amplifies extraction and shifts classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participant_information_asymmetry, empirical, 'Whether participants comprehend timeline-driven safety validation compression').

omega_variable(
    regulatory_expedited_pathway_capture,
    'Are regulatory expedited pathways (breakthrough designation, accelerated approval) being used appropriately for life-threatening conditions, or are they being stretched to accommodate competition timelines for non-emergency age reversal?',
    'Analysis of expedited pathway justifications in competition-linked trials; comparison of approval criteria invoked vs traditional life-threatening-condition standards; assessment of whether competition deadline proximity correlates with pathway approval',
    'If pathways are appropriate: regulatory constraint is coordination. If pathways are stretched: competition is capturing regulatory process and extracting from safety validation requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_expedited_pathway_capture, empirical, 'Whether expedited regulatory pathways are appropriately applied or captured by competition timelines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competition_timeline_pressure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_time_theater_t0, competition_timeline_pressure, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_time_theater_t2, competition_timeline_pressure, theater_ratio, 2, 0.38).
narrative_ontology:measurement(comp_time_theater_t4, competition_timeline_pressure, theater_ratio, 4, 0.44).
narrative_ontology:measurement(comp_time_theater_t6, competition_timeline_pressure, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(comp_time_extract_t0, competition_timeline_pressure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_time_extract_t2, competition_timeline_pressure, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(comp_time_extract_t4, competition_timeline_pressure, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(comp_time_extract_t6, competition_timeline_pressure, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_time_suppress_t0, competition_timeline_pressure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(comp_time_suppress_t2, competition_timeline_pressure, suppression_requirement, 2, 0.54).
narrative_ontology:measurement(comp_time_suppress_t4, competition_timeline_pressure, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(comp_time_suppress_t6, competition_timeline_pressure, suppression_requirement, 6, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competition_timeline_pressure, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of reprogramming_safety_toxicity (mountain — inherent biological limits on reprogramming speed) and regulatory_measurement_gap (rope — coordination problem in defining age reversal metrics). The timeline pressure is a distinct structural constraint reflecting the competition's incentive structure, not the underlying biological or measurement constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competition_timeline_pressure, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
