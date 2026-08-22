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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Doomsday Clock as Strategic Policy Mobilization Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock (established 1947, maintained by the Bulletin of the
 *   Atomic Scientists) sets an annual symbolic position indicating humanity's
 *   distance from existential catastrophe. Officially, the clock's position
 *   reflects expert consensus on global existential and catastrophic risks.
 *   Under the performative-tool reading instantiated here, the clock's
 *   position is strategically chosen to maximize media impact, focus policy
 *   attention, and mobilize collective action on existential risks, with
 *   explicit tolerance for divergence between the clock's stated position and
 *   the underlying risk evidence base. The reading recognizes genuine
 *   coordination benefit (focusing attention on otherwise-neglected risks)
 *   but emphasizes the extraction of epistemic credibility and the
 *   normalization of strategic metric manipulation as costs borne by future
 *   scientific communities. This reading is one of three sister readings of
 *   the doomsday_clock_metric kernel; the other readings frame the clock as
 *   an objective index of measurable risk (objective_index_reading) and as
 *   irreducibly entangled scientific judgment and normative stakes
 *   (hybrid_legitimacy_reading).
 *
 * KEY AGENTS:
 *   - Doomsday Clock Governing Board: institutional agenda-setter; controls the annual position and official interpretation; frames decision-making as synthesis of existing risk indicators but increasingly describes it as calibrated to move the Overton window and generate urgency.
 *   - Existential Risk Policy Advocates: organized beneficiary; use the clock's announcement as platform for policy mobilization; benefit from epistemic authority the clock lends their advocacy.
 *   - Empirical Risk Researchers: moderate-power payer; face pressure to conform their published estimates to the clock's position; bear credibility cost when their evidence diverges from the clock's stated consensus.
 *   - Future Epistemic Communities: non-agent victim; inherit degraded trust environment in which strategic manipulation of high-stakes metrics has become normalized.
 *   - Risk Assessment Methodologists: observer; track divergence between clock position and actual risk evidence; document pattern of strategic positioning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.45).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Strategic Policy Mobilization Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '6803fad2-22aa-4cc2-b126-d7c96600ca75').
narrative_ontology:cs_kernel_codification('6803fad2-22aa-4cc2-b126-d7c96600ca75', formalized).
narrative_ontology:cs_authority_grounding('6803fad2-22aa-4cc2-b126-d7c96600ca75', extraction).
narrative_ontology:cs_interpretation_layer_present('6803fad2-22aa-4cc2-b126-d7c96600ca75').
narrative_ontology:cs_reading_relation('6803fad2-22aa-4cc2-b126-d7c96600ca75', doomsday_clock_metric__objective_index_reading, influences).
narrative_ontology:cs_reading_relation('6803fad2-22aa-4cc2-b126-d7c96600ca75', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('6803fad2-22aa-4cc2-b126-d7c96600ca75', foundational, strategic_positioning_justified_by_policy_stakes).
narrative_ontology:cs_axiom_status(strategic_positioning_justified_by_policy_stakes, holdable).
narrative_ontology:cs_axiom_grounding('6803fad2-22aa-4cc2-b126-d7c96600ca75', strategic_positioning_justified_by_policy_stakes, instrumental).
narrative_ontology:cs_axiom('6803fad2-22aa-4cc2-b126-d7c96600ca75', secondary, clock_position_calibrated_for_attention_impact).
narrative_ontology:cs_axiom_status(clock_position_calibrated_for_attention_impact, holdable).
narrative_ontology:cs_axiom_grounding('6803fad2-22aa-4cc2-b126-d7c96600ca75', clock_position_calibrated_for_attention_impact, empirically_contingent).
narrative_ontology:cs_reference_frame('6803fad2-22aa-4cc2-b126-d7c96600ca75', clock_as_authentic_risk_synthesis).
narrative_ontology:cs_drift_state('6803fad2-22aa-4cc2-b126-d7c96600ca75', contemporary_strategic_positioning_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6803fad2-22aa-4cc2-b126-d7c96600ca75', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, existential_risk_policy_advocates).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_consensus_building).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, media_and_public_discourse).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, media_and_public_discourse).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, empirical_existential_risk_researchers).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, skeptical_scientific_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the clock's hand position annually by consensus among expert members. Officially frames the setting as synthesis of existing existential risk indicators (weapons, climate, biosecurity, AI). Increasingly described in interviews and strategy documents as calibrated to 'move the Overton window' and 'generate urgency' for policy action on existential risks. Controls the authoritative interpretation of what the clock measures.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, doomsday_clock_governing_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the clock's annual announcement as a platform to demand policy action on existential risks. A dramatic hand-movement (toward midnight) amplifies their messaging in media coverage, facilitates fundraising for their organizations, and justifies urgent policy proposals. The clock's prestige as a 'scientific index' lends their advocacy epistemic authority it might not otherwise hold. Benefit from strategic positioning that ties their policy goals to the clock's announced risk assessment.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, existential_risk_policy_advocates, beneficiary,
    organized, generational, mobile, global).

% The annual clock announcement generates reliable, high-stakes news cycles and public engagement around existential risk questions that would otherwise struggle for attention. Media outlets benefit from the dramatic visual and narrative hook. The public gains some attention to low-probability, high-impact risks. However, they also bear the cost of repeated false urgency and desensitization: each year the clock moves closer to midnight without the predicted catastrophes materializing, the signal's credibility erodes for future announcements.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, media_and_public_discourse, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, media_and_public_discourse, payer).

% Conduct systematic research on existential and catastrophic risks using evidence-based methods. Face pressure to conform their published risk estimates to the clock's position (if the clock says 90 seconds to midnight, their own peer-reviewed work faces skepticism if it estimates lower risk). If they publish findings that diverge from the clock's announced position, they are read as either contradicting the 'scientific consensus' or undermining policy urgency. Their credibility as objective researchers becomes entangled with advocacy goals.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, empirical_existential_risk_researchers, payer,
    moderate, biographical, constrained, global).

% Physics departments, risk research centers, and peer-review bodies that maintain distance from the clock's annual statement face pressure to explain why they 'dispute' an apparently authoritative scientific assessment. Institutions emphasizing measurement precision over policy impact must defend their methodological conservatism against accusations of complicity in inaction. The clock's institutional prestige makes dissent costly.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, skeptical_scientific_institutions, payer,
    institutional, generational, constrained, global).

% Inherit a degraded epistemic environment in which strategic manipulation of high-stakes metrics has become normalized. If the clock's hand movements diverge repeatedly from actual risk trajectories, future scientists and policymakers will discount urgent warnings from scientific authorities—a form of cry-wolf cost borne by later generations attempting to mobilize action on actual future catastrophes.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_epistemic_communities, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, future_epistemic_communities).

% Nations and industries that would bear the costs of aggressive existential risk mitigation policies are structurally excluded from the governing board's decision process. They would contest the clock's position as inflated risk assessment designed to justify costly policies without offsetting benefits. Their objections are not invited into the board's reasoning.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_skeptics_and_cost_bearers, excluded,
    powerful, biographical, mobile, global).

% Track the clock's annual positions against actual developments and published risk research to assess calibration. Observe growing divergence between the clock's strategic positioning and the underlying evidence base. Document pattern of hand movements correlating with advocacy needs rather than empirical updates. Lack authority to alter the clock's position but can publish critiques and document the pattern for accountability.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_assessment_methodologists, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, existential_risk_policy_advocates).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Focuses international scientific and policy attention on a defined set of existential and catastrophic risks (nuclear, climate, biosecurity, AI) that would otherwise compete diffusely for resources and political will. A single, annually-updated visual metric provides a common reference point for discourse across scientific, policy, and public domains.
% TRANSFER_FUNCTION: Transfers epistemic authority and policy-mobilization power FROM the open, contestable empirical risk-assessment process TO a centralized body whose stated position becomes the 'scientific consensus' on existential risk. Transfers media attention and public urgency disproportionately to the agenda-setter's preferred risk framings. Extracts credibility-cost FROM future science by normalizing strategic metric manipulation.
% ABSENT_VOICES: Policy skeptics, industries and nations that would bear mitigation costs, methodological conservatives who believe risk is being overstated, and the future epistemic communities who will inherit the degraded trust environment—none sit on the governing board or participate in the setting logic. Their objections to the clock's position are not invited.
% DISAPPEARANCE_RATIONALE: If the doomsday clock ceased to exist, media coverage of existential risks would fragment, policy advocacy for existential risk mitigation would lose its central attention-focusing mechanism, and the board members would lack their annual platform to mobilize urgency. The constraint's persistence directly enables the policy-mobilization strategy; without it, advocates would need to rely on distributed evidence-building, which is slower and less dramatic.
% FOUNDING_PROBLEM: Existential and catastrophic risks (nuclear weapons, climate, biosecurity, AI) are genuine but difficult to communicate at scale; they compete poorly with immediate concerns for public and political attention; scientific evidence about these risks is distributed across disciplines and institutions without a common frame.
% FOUNDING_PROBLEM_CORROBORATION: Risk researchers and policy advocates attest the founding problem is live—existential risks remain under-weighted in policy allocation relative to their probability and impact. However, independent risk methodologists and epistemologists outside the advocacy ecosystem also attest that the founding problem has been increasingly supplemented by a secondary problem: the clock has become a strategic tool whose positioning diverges from the underlying risk assessment, creating a new failure mode (cry-wolf, credibility erosion) that was not part of the original charter.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   This reading authors high extractiveness (0.68 at interval end) because the constraint extracts epistemic authority and credibility from the underlying risk-assessment process by centralizing what is framed as 'scientific consensus' but is increasingly driven by strategic positioning for policy impact. Suppression is moderate (0.45) because the constraint operates through public narrative and institutional prestige rather than coercive exclusion, though methodological dissent is costly and skeptical voices are excluded from the governing structure. Theater is very high (0.72 at interval end), reflecting the measurement series' steep rise: the clock's function has increasingly shifted from indicator to performative tool. The series shows the constraint's evolution from a modest risk-communication device (1947: theater ~0.08) to a strategic policy-mobilization instrument (2026: theater ~0.72). This trajectory tracks documented board statements emphasizing strategic positioning and public impact over calibration to underlying evidence. The temporal grid is shared: all three metrics are authored at six points spanning the constraint's 79-year history.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (governing board) and the beneficiary (policy advocates) should compute as coordinated or low-extraction from their seats: the clock enables genuine policy mobilization they endorse, and they view strategic positioning as justified by the stakes of existential risk. Empirical risk researchers and future epistemic communities, by contrast, should compute as substantially extracted: they bear the cost of normalized metric manipulation and degraded future trust without benefiting from the mobilization. Risk methodologists sit as observers, documenting the divergence. The engine computes these divergences from directionality (power + exit options + beneficiary/victim declarations). This story does not reconcile the perspectives; it names the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The board has powerful institutional position and arbitrage-grade exit (can cease participating; can shift the focus; retains prestige). Low directionality (d near 0.2) → low/negative effective extraction for this seat. Policy advocates have organized power and mobile exit; they benefit from the constraint; d near 0.25. Empirical researchers have moderate power, constrained exit (can dissent but at credibility cost), and are named as victims; d near 0.7. Future epistemic communities have powerless institutional position, trapped exit, and bear the credibility cost; d approaches 1.0. The board's enforcement requirement is moderate because the constraint operates through narrative authority and institutional prestige, not through coercive exclusion (though dissent is costly).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of drift toward mandatrophy—the founding problem (attention deficit for existential risks) is live, but the constraint's function increasingly diverges from solving that problem. Early in the interval (1947–1980), the clock was a low-theater device for synthesizing existing risk assessments. By 2010–2026, the theater ratio has climbed steeply, indicating that performative mobilization has increasingly decoupled from evidence synthesis. The constraint persists because it serves advocacy goals and enables policy mobilization, not because it solves the original attention-allocation problem more effectively. A tangled_rope classification captures this: genuine coordination function (focusing attention on neglected risks) coexists with asymmetric extraction (credibility cost borne by future science, policy constraints on dissent). If theater continues rising while accuracy diverges, the constraint approaches piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evidence_base_divergence,
    'How far does the clock''s stated position diverge from a systematic synthesis of published empirical risk research across domains (weapons, climate, biosecurity, AI)?',
    'Quantitative meta-analysis: compile published risk estimates across the four domains; compute a weighted consensus position; compare to the clock''s annual position. Repeat every two years to establish divergence trajectory.',
    'If divergence is systematic and widening, the clock is extracting credibility from the evidence base to serve strategic positioning; classification trends toward pure snare. If divergence remains small and random, the clock''s position remains plausibly grounded in evidence synthesis, and the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_base_divergence, empirical, 'Calibration gap between clock position and published risk consensus.').

omega_variable(
    suppression_of_dissent,
    'To what extent is skeptical commentary on the clock''s position suppressed through institutional or reputational mechanisms?',
    'Survey of risk researchers and methodologists on costs of public dissent; analysis of institutional pressure applied to researchers whose published estimates diverge from clock position; documentation of board responses to external critiques.',
    'If suppression is substantial and deliberate, suppression metric should rise toward 0.65–0.75, indicating this is a snare rather than tangled_rope. If suppression is low and skepticism is publicly engaged, the open-contestation framing is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_dissent, empirical, 'Institutional suppression of methodological dissent on clock calibration.').

omega_variable(
    strategic_positioning_acknowledgment,
    'Do board statements and internal documents acknowledge that clock positioning is calibrated for policy impact independently of evidence synthesis, or is strategic positioning denied?',
    'FOIA requests for board meeting minutes; interviews with current and former board members on whether position reflects evidence or policy goals; textual analysis of official statements over the interval.',
    'If strategic positioning is openly acknowledged, the constraint may warrant reclassification under different axioms (less focus on epistemic extraction, more on explicit coordination of policy goals). If it is denied while evidence shows otherwise, the extraction is higher because it includes denial of the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_positioning_acknowledgment, empirical, 'Transparency of strategic positioning rationale in board communications.').

omega_variable(
    future_credibility_cost_mechanism,
    'Is the cost to future epistemic credibility from normalized metric manipulation structural (inevitable given the constraint''s design) or contingent (avoidable if the board adopts more conservative calibration)?',
    'Counterfactual analysis: model future risk-communication landscape under scenarios of high vs. low clock-manipulation history. Test whether future scientists and publics actually discount subsequent urgent warnings based on past clock divergence, or whether the cost is theoretical.',
    'If the cost is structural and inevitable, this constraint imposes a permanent degradation on future science''s ability to mobilize action on actual catastrophes—a civilizational extraction. If avoidable, the cost is conditional on the board''s strategic choices, and reclassification depends on choices made.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_credibility_cost_mechanism, conceptual, 'Whether credibility cost to future science is structural or choice-contingent.').

omega_variable(
    alternative_attention_mechanism,
    'Are there plausible alternative coordination mechanisms (without the clock) that would achieve similar attention-focusing and policy mobilization without the epistemic extraction?',
    'Design study: sketch alternative institutions for coordinating risk research and policy (distributed risk-index, multi-stakeholder governance with transparent disagreement, real-time evidence dashboards). Assess whether they would achieve similar policy impact without centralizing epistemic authority.',
    'If strong alternatives exist, the clock''s extraction is not the only way to solve the original attention-allocation problem, and the constraint may be reclassifiable as pure snare. If no plausible alternative achieves equal coordination benefit, the tangled_rope classification is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_attention_mechanism, conceptual, 'Whether the coordination benefit requires the clock''s specific epistemic structure or whether alternatives exist.').

omega_variable(
    kernel_reading_underdetermination,
    'Is this reading (performative_tool) distinguishable from the hybrid_legitimacy reading on structural grounds, or do they converge in practice?',
    'Comparative analysis: the hybrid_legitimacy_reading refuses the separation between objective measurement and strategic positioning, treating them as irreducibly entangled. This reading separates them and asserts strategic positioning is an extraction. The readings foreclose each other only if one denies the other a coherent framework. Documentary evidence on board reasoning can show whether strategic positioning is acknowledged as distinct or genuinely inseparable.',
    'If the readings are genuinely incommensurable (non-overlapping axioms), the three-reading framework is valid. If they converge or diverge only on framing, the kernel might require recombination or reclassification of sibling relationships.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Structural distinctness of performative_tool reading from hybrid_legitimacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement_basis(doom_tr_t1947, observed).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(doom_tr_t1980, observed).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(doom_tr_t2000, observed).
narrative_ontology:measurement(doom_tr_t2010, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(doom_tr_t2010, observed).
narrative_ontology:measurement(doom_tr_t2018, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2018, 0.64).
narrative_ontology:measurement_basis(doom_tr_t2018, observed).
narrative_ontology:measurement(doom_tr_t2026, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2026, 0.72).
narrative_ontology:measurement_basis(doom_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement_basis(doom_be_t1947, observed).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement_basis(doom_be_t1980, observed).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(doom_be_t2000, observed).
narrative_ontology:measurement(doom_be_t2010, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement_basis(doom_be_t2010, observed).
narrative_ontology:measurement(doom_be_t2018, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement_basis(doom_be_t2018, observed).
narrative_ontology:measurement(doom_be_t2026, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(doom_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.12).
narrative_ontology:measurement_basis(doom_su_t1947, observed).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement_basis(doom_su_t1980, observed).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement_basis(doom_su_t2000, observed).
narrative_ontology:measurement(doom_su_t2010, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(doom_su_t2010, observed).
narrative_ontology:measurement(doom_su_t2018, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(doom_su_t2018, observed).
narrative_ontology:measurement(doom_su_t2026, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2026, 0.45).
narrative_ontology:measurement_basis(doom_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.06).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel is decomposed into three reading-dependent constraint stories per the ε-invariance principle. Each reading instantiates a different constraint with distinct ε values, beneficiary/victim structures, and classifications. The objective_index_reading treats the clock as measurement (low ε, genuine coordination). The hybrid_legitimacy_reading refuses the measurement/positioning distinction (moderate ε, irreducible entanglement). The performative_tool_reading (this story) treats positioning as strategic, extracting credibility for policy impact (high ε, tangled_rope). The three readings coexist as live positions held by different institutional actors (governance board, risk researchers, hybrid epistemologists); the network edges model their structural influence. The performative_tool_reading's high theater ratio and rising trajectory over time create pressure on the other readings' legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
