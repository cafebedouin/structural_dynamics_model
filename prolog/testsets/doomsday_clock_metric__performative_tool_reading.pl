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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock Strategic Metric Setting (Performative Tool Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Doomsday Clock is presented to the
 *   public as a precise indicator of humanity's proximity to existential
 *   catastrophe, set by expert consensus on measurable risk indicators
 *   (nuclear stockpiles, climate trajectories, biotech proliferation, AI
 *   development). This reading instantiates the PERFORMATIVE TOOL
 *   interpretation of the Clock: the metric's annual setting is strategically
 *   chosen to maximize media attention and mobilize policy action on
 *   existential risks, with the authority of 'scientific judgment' lending
 *   credibility to what is fundamentally an advocacy positioning. The
 *   constraint benefits risk activists and policy coalitions seeking to
 *   accelerate action; it extracts from the scientific credibility commons
 *   and mortgages future risk communication authority. The measurement series
 *   and high theater_ratio (0.79 at interval end) reflect the escalating
 *   performative component: Clock advances correlate increasingly with
 *   advocacy campaigns rather than measured risk escalation.
 *
 * KEY AGENTS:
 *   - doomsday_clock_stewards (agenda-setter, institutional): determine the annual setting; frame Clock advances as scientific consensus; strategically choose risks to feature
 *   - existential_risk_advocacy_movement (beneficiary, organized): use Clock advances as rhetorical signal; fund-raising and policy leverage depend on Clock salience
 *   - nuclear_policy_reformers (beneficiary, powerful): leverage Clock authority in disarmament negotiations; accept reputational risk if Clock credibility erodes
 *   - climate_action_coalitions (beneficiary, organized): use Clock to justify acceleration of carbon policy; depend on perceived objectivity
 *   - scientific_credibility_commons (victim, powerless): epistemic authority erodes as public learns Clock setting is strategic; long-term cost to all future scientific warnings
 *   - future_risk_communication_authority (victim, powerless): capacity to mobilize on real existential risks diminished when metric credibility breaks
 *   - science_journalists_and_media (excluded): amplify Clock signal; would demand transparency but are not seated in setting decision
 *   - contrarian_risk_skeptics (excluded): question existential risk quantification; excluded from decision-making; would challenge metric weighting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.71).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.79).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.79).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Strategic Metric Setting (Performative Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '2f808285-bd2e-44c6-bcce-d162f5075aa3').
narrative_ontology:cs_kernel_codification('2f808285-bd2e-44c6-bcce-d162f5075aa3', fixed_text).
narrative_ontology:cs_authority_grounding('2f808285-bd2e-44c6-bcce-d162f5075aa3', extraction).
narrative_ontology:cs_interpretation_layer_present('2f808285-bd2e-44c6-bcce-d162f5075aa3').
narrative_ontology:cs_reading_relation('2f808285-bd2e-44c6-bcce-d162f5075aa3', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('2f808285-bd2e-44c6-bcce-d162f5075aa3', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('2f808285-bd2e-44c6-bcce-d162f5075aa3', foundational, strategic_signaling_legitimate_for_existential_mobilization).
narrative_ontology:cs_axiom_status(strategic_signaling_legitimate_for_existential_mobilization, holdable).
narrative_ontology:cs_axiom_grounding('2f808285-bd2e-44c6-bcce-d162f5075aa3', strategic_signaling_legitimate_for_existential_mobilization, instrumental).
narrative_ontology:cs_axiom('2f808285-bd2e-44c6-bcce-d162f5075aa3', foundational, measurement_authority_subordinate_to_policy_necessity).
narrative_ontology:cs_axiom_status(measurement_authority_subordinate_to_policy_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2f808285-bd2e-44c6-bcce-d162f5075aa3', measurement_authority_subordinate_to_policy_necessity, deontological).
narrative_ontology:cs_reference_frame('2f808285-bd2e-44c6-bcce-d162f5075aa3', scientific_existential_risk_measurement).
narrative_ontology:cs_drift_state('2f808285-bd2e-44c6-bcce-d162f5075aa3', contemporary_advocacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f808285-bd2e-44c6-bcce-d162f5075aa3', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_movement).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, nuclear_policy_reformers).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, climate_action_coalitions).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_credibility_commons).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_risk_communication_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_audiences).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, nuclear_policy_reformers).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, policy_audiences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bulletin of the Atomic Scientists maintains the Clock as a symbolic metric and strategically adjusts its annual setting. They frame the Clock as tracking existential risk through expert consensus; internally, they acknowledge that the setting is chosen partly to maintain cultural salience and mobilize policy attention. They determine which risks (nuclear, climate, biotechnology, AI) are featured and weighted in the annual decision.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, doomsday_clock_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Activist coalitions seeking to elevate existential and catastrophic risk on policy agendas use the Clock's movement as amplified signal to media and policymakers. Each advance toward midnight strengthens their rhetorical position and increases funding for existential risk research and advocacy. They benefit from the Clock's authority as an 'objective scientific judgment' even while understanding the performative element.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_movement, beneficiary,
    organized, generational, mobile, global).

% Government and institutional actors working to reduce nuclear weapons stockpiles and tighten non-proliferation regimes use Clock advances as rhetorical leverage in negotiations. They both benefit from the Clock's mobilizing effect and bear a reputational cost if the Clock's credibility erodes when advances are seen as activist rather than expert-driven.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, nuclear_policy_reformers, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, nuclear_policy_reformers, payer).

% Environmental and climate organizations leverage Clock advances that incorporate climate risk to signal urgency and justify accelerated action policies. They use the metric's apparent authority to overcome political resistance to carbon policy. Like the existential risk movement, they benefit from the Clock's framing as objective scientific judgment.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, climate_action_coalitions, beneficiary,
    organized, generational, mobile, global).

% The epistemic authority of scientific risk assessment degrades over time as the public and policymakers learn (or come to believe) that the Clock is strategically manipulated. This erosion affects not only the Clock itself but the credibility of future scientific warnings about actual existential risk. Once trust in the metric breaks, distinguishing genuine escalations from advocacy messaging becomes harder for lay audiences.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_credibility_commons, payer,
    powerless, civilizational, trapped, global).

% The capacity of future scientific institutions to mobilize collective action on real existential risks is compromised if the Clock's strategic setting becomes common knowledge. When a metric is exposed as performative, its later use for genuine warning loses potency. The constraint's enforcement trades future communication authority for present policy activation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_risk_communication_authority, payer,
    powerless, civilizational, trapped, global).

% Media covering the Clock announcement face pressure to report it as breaking news ('Clock moves closer to midnight') while gradually recognizing the strategic framing. They are excluded from the setting decision itself but their coverage amplifies the Clock's signal; they would demand transparency about the decision-making process but are not seated at the table.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_journalists_and_media, excluded,
    organized, biographical, constrained, global).

% Scholars and commentators who question existential risk quantification or advocate for different policy priorities are excluded from the Clock setting process. They would argue the metric is distorted by activism and that other risks are under-weighted or over-weighted; their voice in the decision-making would challenge the consensus framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, contrarian_risk_skeptics, excluded,
    moderate, biographical, mobile, global).

% Policymakers and governments use Clock advances as external justification for policies they favor on independent grounds (accelerating nuclear disarmament, climate action). They benefit from the metric's apparent objectivity lending legitimacy to their agenda. They also risk being manipulated if the Clock's strategic nature becomes salient to political opponents.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_audiences, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, policy_audiences, payer).

% Meta-level observers tracking the epistemic structure of risk communication and the boundaries between expert judgment and advocacy positioning.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, existential_risk_advocacy_movement).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate global expert judgment on existential risk levels and translate technical risk assessment into a salient cultural symbol that penetrates policy discourse and public attention. The Clock solves the problem that technical risk papers circulate within expert communities but fail to mobilize action.
% TRANSFER_FUNCTION: Transfers epistemic authority (the credibility of being 'scientific judgment') from the risk assessment process to policy activism. The Clock's annual decision redirects scientific prestige toward policy goals chosen partly for mobilization value rather than pure risk measurement. In exchange, risk advocates gain powerful rhetorical leverage.
% ABSENT_VOICES: Contrarian risk skeptics, scholars questioning existential risk quantification, and voices advocating different risk priorities (pandemic preparedness, biodefense, inequality) are excluded from the setting decision. They would challenge the metric's weighting and the activist framing but are not seated when the Clock is set.
% DISAPPEARANCE_RATIONALE: If the Clock vanished, existential and catastrophic risk would receive less regular media amplification and cultural salience. Policy pressure on nuclear disarmament and climate action would decline without the symbolic trigger. Risk advocacy organizations would lose a primary communication tool. However, the underlying risks and policy coalitions would persist; the Clock is a crucial mobilization mechanism, not the foundation.
% FOUNDING_PROBLEM: In the 1940s, nuclear weapons scientists needed a way to communicate the existential threat they had created to the broader public and policymakers, beyond technical papers. The Clock was designed as a simple symbolic representation of how close humanity stood to civilization-ending catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists originally attests the founding problem as live and the Clock as its solution. Contemporary scholars of science communication (Sontag, Latour, Jasanoff) document how the Clock has evolved from risk indicator toward rhetorical tool. Policy analysts outside the advocacy movement note that Clock advances now correlate with activist campaigns rather than measured changes in objective risk indicators — this external corroboration supports the shift-of-function reading.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).

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
 *   This reading positions the Clock as a tangled_rope: genuine coordination problem (translating technical risk assessment into policy-salient signal) combined with asymmetric extraction (beneficiaries are policy advocates; victims are future credibility). Extractiveness is high (0.68) and rising over the interval because Clock advances increasingly decouple from independent risk measurement and track advocacy campaign cycles. Theater ratio rises from 0.35 to 0.79 as the performative element dominates; this is NOT a sign the constraint is becoming less real — it is evidence that the coordination function (translating risk into cultural signal) is increasingly subsumed by strategic signaling. Suppression (0.71) reflects the active enforcement required to maintain the public's perception of the Clock as objective scientific judgment while the stewards are making strategic choices; the suppression is mainly reputational and communicative (managing how the Clock's setting is framed) rather than coercive. Accessibility_collapse (0.42) is moderate-low because alternative framings of existential risk remain available and some audiences are aware of the Clock's strategic element; the constraint does not foreclose rival risk narratives entirely, though it does dominate media coverage. Resistance (0.58) is moderate because existential risk skeptics and contrarian voices maintain active pushback, and some policymakers resist using the Clock as a decision basis (preferring independent risk assessment).
 *
 * PERSPECTIVAL GAP:
 *   From the stewards' seat, the Clock is coordinate-and-communicate: they solve the real problem of translating expert judgment into cultural signal, and the strategic element is a necessary component of the solution (if the setting were purely mechanical, no one would pay attention; strategic framing is what makes communication work). From the advocacy movement's seat, the Clock is mobilization leverage with borrowed authority: they benefit from the metric's false appearance of objectivity while knowing it is chosen partly for impact. From the scientific credibility commons' seat (which has no agent to speak for it), the Clock is extraction: present benefit to policy (faster action on real risks) is purchased with future cost (erosion of the capacity to warn). The engine computes per-seat directionality from the structural data: stewards should compute as beneficiaries (d low), advocacy movements as beneficiaries (d low), scientific commons as victims (d high), future risk communicators as victims (d high). The divergence between the CLAIMED type (tangled_rope: coordination + enforcement) and what different seats experience (stewards: rope; advocates: snare with cover story; victims: pure extraction with no coordination benefit) is the measurement the schema exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Stewards (institutional power, analytical exit) are structural beneficiaries: they maintain institutional control, secure funding, and accomplish their mission of elevating existential risk on policy agendas. Directionality d ≈ 0.2 (near beneficiary end). Existential risk advocates and climate coalitions (organized power, mobile exit) are also beneficiaries: they gain rhetorical leverage and policy amplification; if the Clock's credibility erodes, they can exit to other communication tools. d ≈ 0.25 for these groups. Nuclear policy reformers (powerful, constrained by political feasibility) are dual: they benefit from the Clock's mobilizing effect but bear reputational cost if credibility breaks; d ≈ 0.45 (closer to symmetric). Scientific credibility commons (powerless, trapped) has no exit and bears the full cost of credibility erosion; d ≈ 0.95 (near full target). Future risk communication authority (powerless, trapped) is similarly at the target end. Policy audiences (powerful, arbitrage exit) can choose whether to depend on Clock framing; d ≈ 0.4 (moderate — they capture some benefit, bear some risk if Clock's credibility breaks). Excluded voices (contrarian skeptics, journalists) are neither beneficiaries nor explicit payers; they are structurally prevented from influencing the constraint. This distribution — concentrated benefit at institutional and organized level, diffuse long-term cost at the commons level — is characteristic of tangled_rope with substantial asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Clock's founding problem was urgent and real: in the 1940s and through the Cold War, communicating nuclear existential risk to the public and policymakers was a genuine coordination problem. The problem remains live in one sense: existential risks (nuclear, climate, biotech, AI) are real and policy attention is still inadequate. However, the founding problem and the clock's function have begun to diverge. The founding problem was 'how do we communicate that nuclear weapons pose an existential threat?' The Clock solves this by creating a symbolic cultural reference point. But the Clock's FUNCTION has evolved: it now coordinates policy attention on a broader set of existential risks, and the setting is chosen strategically to maximize policy impact, not purely to measure risk level. The constraint thus exhibits early mandatrophy: the founding mandate (measure and communicate existential risk) persists nominally, but the actual operation has shifted toward a different function (strategic signaling to mobilize policy action). This is NOT yet terminal mandatrophy (the founding problem is not dead, and the Clock still serves a real coordination role), but the gap is widening. The measurement series shows theater_ratio rising from 0.35 to 0.79, indicating the performative component increasingly dominates. If theater_ratio reaches 0.90+, and if research shows Clock advances no longer track independent risk indicators, mandatrophy would be effectively complete: the constraint would persist via institutional inertia and because the advocacy coalitions benefit from it, but the founding mandate would be clearly superseded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_vs_measurement_intent,
    'Do the Clock stewards explicitly intend each annual setting as a strategic choice to maximize policy impact, or do they experience the setting as emerging from risk measurement with strategic communication as a secondary justification?',
    'Internal decision-making documentation, interviews with stewards about decision criteria, analysis of whether Clock adjustments correlate with changes in measured risk indicators vs. changes in advocacy campaign priorities.',
    'If setting is explicitly strategic, the constraint is squarely tangled_rope (coordination function + asymmetric extraction of credibility). If stewards believe they are measuring and communicating, the constraint might be rope (genuine coordination with incidental extraction). The reading hinges on the stewards'' actual epistemic position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_measurement_intent, empirical, 'Whether Clock-setting is intentionally strategic or believed-to-be-measurement with strategic side effect.').

omega_variable(
    credibility_erosion_timeline,
    'At what point does public and expert knowledge of the Clock''s strategic setting undermine its future mobilizing capacity? Is there a threshold beyond which the metric''s effectiveness inverts?',
    'Survey data on public and policymaker perception of Clock objectivity over time; analysis of whether Clock advances continue to drive policy attention as common knowledge of strategic framing spreads; counterfactual: would a transparent hybrid-legitimacy reading (acknowledging both measurement and advocacy) preserve more long-term credibility than the current performative framing?',
    'If erosion threshold is near, the constraint extracts from future-communication authority faster than present-policy benefit accrues, converting tangled_rope toward snare. If threshold is far, extraction can persist longer. This omega determines whether mandatrophy is incipient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_erosion_timeline, empirical, 'Timeline to credibility collapse and inversion of the Clock''s policy effect.').

omega_variable(
    counterfactual_policy_momentum,
    'How much of the observed policy movement on nuclear disarmament and climate action would occur without the Clock''s rhetorical leverage? Is the Clock a necessary mobilizer or an amplifier of pre-existing momentum?',
    'Comparative analysis of policy movement in jurisdictions where the Clock has high cultural salience vs. where it has low salience; interviews with policymakers about decision drivers; analysis of whether policy advances correlate with Clock movements or with independent shifts in cost-benefit calculations.',
    'If policy would occur anyway, the Clock''s coordination function is marginal and extraction is the dominant dynamic (snare candidate). If the Clock is necessary to overcome political resistance, coordination function is real and tangled_rope claim holds. The beneficiary structure (advocacy movements vs. policy outcomes) depends on this counterfactual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_policy_momentum, empirical, 'Causal necessity of the Clock in observed policy momentum.').

omega_variable(
    kernel_reading_contest,
    'This constraint is ONE reading of the doomsday_clock_metric kernel. How does the performative-tool reading''s core premise (strategic manipulation is legitimate if mobilization is necessary) relate structurally to the sibling readings?',
    'Logical analysis of whether the three readings (performative_tool, objective_index, hybrid_legitimacy) can be held simultaneously within a single authority framework, or whether they are genuinely foreclosed against each other.',
    'If performative_tool forecloses objective_index (the premise that measurement is strategic rules out the premise that measurement is objective), the kernel exhibits unresolvable internal contradiction and mandates institutional choice. If they coexist within different communities, the constraint''s legitimacy is community-dependent. The reading relations determine whether the kernel is stable or under structural pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relations between this reading and sibling readings of the doomsday clock metric kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(doom_tr_t0, observed).
narrative_ontology:measurement(doom_tr_t5, doomsday_clock_metric__performative_tool_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(doom_tr_t5, observed).
narrative_ontology:measurement(doom_tr_t10, doomsday_clock_metric__performative_tool_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement_basis(doom_tr_t10, observed).
narrative_ontology:measurement(doom_tr_t15, doomsday_clock_metric__performative_tool_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(doom_tr_t15, observed).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__performative_tool_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(doom_tr_t20, observed).
narrative_ontology:measurement(doom_tr_t25, doomsday_clock_metric__performative_tool_reading, theater_ratio, 25, 0.75).
narrative_ontology:measurement_basis(doom_tr_t25, observed).
narrative_ontology:measurement(doom_tr_t30, doomsday_clock_metric__performative_tool_reading, theater_ratio, 30, 0.77).
narrative_ontology:measurement_basis(doom_tr_t30, observed).
narrative_ontology:measurement(doom_tr_t35, doomsday_clock_metric__performative_tool_reading, theater_ratio, 35, 0.79).
narrative_ontology:measurement_basis(doom_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(doom_be_t0, observed).
narrative_ontology:measurement(doom_be_t5, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(doom_be_t5, observed).
narrative_ontology:measurement(doom_be_t10, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(doom_be_t10, observed).
narrative_ontology:measurement(doom_be_t15, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(doom_be_t15, observed).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(doom_be_t20, observed).
narrative_ontology:measurement(doom_be_t25, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(doom_be_t25, observed).
narrative_ontology:measurement(doom_be_t30, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(doom_be_t30, observed).
narrative_ontology:measurement(doom_be_t35, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(doom_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(doom_su_t0, observed).
narrative_ontology:measurement(doom_su_t5, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(doom_su_t5, observed).
narrative_ontology:measurement(doom_su_t10, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(doom_su_t10, observed).
narrative_ontology:measurement(doom_su_t15, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(doom_su_t15, observed).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(doom_su_t20, observed).
narrative_ontology:measurement(doom_su_t25, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(doom_su_t25, observed).
narrative_ontology:measurement(doom_su_t30, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(doom_su_t30, observed).
narrative_ontology:measurement(doom_su_t35, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(doom_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.18).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, existential_risk_epistemic_authority).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, scientific_credibility_commons_erosion).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three constraint stories with structurally distinct ε values. This reading (performative_tool) treats the Clock's setting as a strategic choice for policy mobilization, with high tolerance for decoupling the announced setting from independent risk measurement. The objective_index reading treats the setting as emerging from risk measurement. The hybrid_legitimacy reading acknowledges both elements and treats the legitimacy question as irreducible. Each reading has a different beneficiary structure, different victims, and different classification from the same underlying metric. The three stories are linked via affects_constraints to enable comparative analysis of how different framings of the Clock's function produce different constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
