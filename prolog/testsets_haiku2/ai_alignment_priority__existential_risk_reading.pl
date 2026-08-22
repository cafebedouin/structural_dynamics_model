% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Existential Risk Alignment Priority
 *   domain: technology_ethics/ai_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates the existential-risk reading of the
 *   contested kernel 'AI alignment priority.' Alignment, in this reading,
 *   means preventing catastrophic loss of control over advanced AI systems
 *   where the loss would be irreversible and humanity-ending. The priority is
 *   existential safety: research should focus on understanding and
 *   controlling future superintelligence before it arrives. This reading
 *   competes with the nearterm-harms reading (alignment as preventing present
 *   discriminatory/extractive harms) and the integrated reading (alignment as
 *   addressing both). The existential-risk reading dominates current funding
 *   and research agendas despite being contested on whether the founding
 *   problem (loss of control over superintelligence) is a live, addressable,
 *   or primary concern. The constraint operates by directing resources,
 *   prestige, and institutional legitimacy toward capability research and
 *   formal alignment work, subordinating present-harm mitigation in the
 *   process.
 *
 * KEY AGENTS:
 *   - Capability research institutions: set research agenda around superintelligence control; receive funding and prestige
 *   - Alignment researchers: benefit from resource flow, publication venues, career opportunities under existential-risk framing
 *   - Longtermist funding bodies: allocate resources by existential-risk criterion; actively enforce priority via grant decisions
 *   - Present-harm researchers: structurally excluded, resource-constrained, must adopt existential-risk language to secure funding
 *   - Present harm impacted populations: posited as beneficiaries but bear cost of de-prioritized present-day safety work
 *   - AI capability labs: benefit from legitimation of rapid capability advancement under existential-risk language
 *   - Policy makers: inherit the existential-risk framing when designing governance; face institutional inertia and expert deference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Existential Risk Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology_ethics/ai_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '38af9b3f-2569-4a14-8abe-65b384ce0304').
narrative_ontology:cs_kernel_codification('38af9b3f-2569-4a14-8abe-65b384ce0304', distributed).
narrative_ontology:cs_authority_grounding('38af9b3f-2569-4a14-8abe-65b384ce0304', extraction).
narrative_ontology:cs_interpretation_layer_present('38af9b3f-2569-4a14-8abe-65b384ce0304').
narrative_ontology:cs_reading_relation('38af9b3f-2569-4a14-8abe-65b384ce0304', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('38af9b3f-2569-4a14-8abe-65b384ce0304', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('38af9b3f-2569-4a14-8abe-65b384ce0304', foundational, existential_risk_is_primary_moral_concern).
narrative_ontology:cs_axiom_status(existential_risk_is_primary_moral_concern, holdable).
narrative_ontology:cs_axiom_grounding('38af9b3f-2569-4a14-8abe-65b384ce0304', existential_risk_is_primary_moral_concern, deontological).
narrative_ontology:cs_axiom('38af9b3f-2569-4a14-8abe-65b384ce0304', foundational, superintelligence_control_problem_tractable_via_capability_research).
narrative_ontology:cs_axiom_status(superintelligence_control_problem_tractable_via_capability_research, holdable).
narrative_ontology:cs_axiom_grounding('38af9b3f-2569-4a14-8abe-65b384ce0304', superintelligence_control_problem_tractable_via_capability_research, empirically_contingent).
narrative_ontology:cs_axiom('38af9b3f-2569-4a14-8abe-65b384ce0304', secondary, present_harms_secondary_to_existential_prevention).
narrative_ontology:cs_axiom_status(present_harms_secondary_to_existential_prevention, holdable).
narrative_ontology:cs_axiom_grounding('38af9b3f-2569-4a14-8abe-65b384ce0304', present_harms_secondary_to_existential_prevention, instrumental).
narrative_ontology:cs_reference_frame('38af9b3f-2569-4a14-8abe-65b384ce0304', existential_risk_dominance_framework).
narrative_ontology:cs_drift_state('38af9b3f-2569-4a14-8abe-65b384ce0304', contemporary_funding_and_research_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38af9b3f-2569-4a14-8abe-65b384ce0304', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, humanity_undifferentiated).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_harm_impacted_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, funding_bodies_longtermist).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, humanity_undifferentiated).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, ai_capability_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research organizations and their funders prioritize developing advanced AI capabilities under the frame that understanding and controlling future systems requires building them. They set the research agenda around existential risk reduction via capability research. They receive funding, research prestige, and directional influence over AI development priorities. Their justification is that existential risk dominates all other concerns.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, capability_research_institutions, agenda_setter,
    institutional, civilizational, mobile, global).

% Researchers in formal verification, interpretability, and control mechanisms receive funding, publication venues, and institutional status by adopting the existential-risk framing. They benefit from the resource flow and credibility granted to work that treats existential risk as primary. They can publish, fund junior researchers, and build careers around this priority ordering.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, alignment_researchers, beneficiary,
    powerful, civilizational, arbitrage, global).

% Major philanthropic funders adopt existential-risk framing as their primary allocation criterion. They set grant priorities, convene researcher networks, and establish research agendas. They benefit from a coherent moral narrative and a measurable strategic mission. They actively enforce the priority by withholding funding from research they view as unaligned with existential-risk focus.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, funding_bodies_longtermist, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, funding_bodies_longtermist, beneficiary).

% All humans are posited as the beneficiary: existential-risk research claims to reduce catastrophic loss-of-control scenarios that would harm everyone. Simultaneously, the resource flow to existential-risk research is a cost borne by humanity's present opportunity set—funding directed to long-term speculative risks is capital unavailable for addressing present harms. The victim status is imposed by the constraint's framing; most people are not consulted on whether they accept this beneficiary/victim bundling.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, humanity_undifferentiated, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, humanity_undifferentiated, payer).

% Populations experiencing discriminatory AI deployment, economic displacement, and algorithmic harms in the present have claims on AI safety resources. The existential-risk priority systematically subordinates their harm to speculative future scenarios. They bear the cost of unfunded present-day harm mitigation and are often excluded from research agendas focused on long-term risk. Their exit options are structurally non-existent—the allocation decisions are made by institutions they do not control.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_harm_impacted_populations, payer,
    powerless, biographical, trapped, global).

% Researchers working on present-day discriminatory AI systems, labor displacement, and algorithmic bias face resource scarcity and lower institutional status under the existential-risk framing. They must either adopt existential-risk language to secure funding (identity lock) or remain under-resourced. Their exclusion from agenda-setting means their research priorities are not included in the constraint's core structure. They have exit options (alternative funding, policy work, industry positions) but constrained ones due to the prestige and grant economy favoring existential-risk framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, excluded).

% Large AI labs benefit from the existential-risk framing because it legitimizes rapid capability development under the language of safety research. They can frame competitive capability advancement as necessary for understanding and controlling future systems. The constraint provides cover for scaling that would otherwise face scrutiny. They benefit from increased funding and reduced external pressure on capability timelines.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_capability_labs, beneficiary,
    institutional, biographical, mobile, global).

% Government actors and regulators observe the research and funding priorities set by the existential-risk reading. They inherit the framing when designing policy: if existential risk dominates, policy focuses on preventing misaligned superintelligence rather than on regulating present-day harms. They take testimony from researchers, review funding priorities, and design governance accordingly. Their analytical position means they have the formal power to alter the constraint via policy but face institutional inertia and expert deference.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_makers_and_regulators, observer,
    institutional, generational, analytical, national).

% Not an agent (no voice or agency in present decisions), but named as the beneficiary in the existential-risk framing. The constraint is justified as securing a future in which advanced AI does not destroy humanity. Future generations cannot participate in allocation decisions, consent to the trade-offs, or object to the present prioritization of speculative risks over known harms.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research effort around a shared frame: existential risk from loss of control over advanced AI is the dominant moral concern. Researchers across institutions work toward the shared goal of understanding and preventing catastrophic misalignment. Funding bodies allocate resources around this unified priority. This coordination solves the problem of dispersed research effort by centralizing the objective.
% TRANSFER_FUNCTION: Moves research funding, institutional attention, publication prestige, and career opportunities from present-harm domains (algorithmic discrimination, labor displacement, surveillance harms) to long-term-risk domains (interpretability, formal verification, alignment research). The constraint transfers the moral and material priority status from present suffering to speculative future scenarios.
% ABSENT_VOICES: Populations experiencing present algorithmic harms are structurally absent from the constraint's core research agendas and funding decisions. They would object that their documented suffering is subordinated to speculative scenarios. Global South populations affected by extractive AI-enabled labor systems and algorithmic bias are not represented in the institutions setting the existential-risk agenda. Future generations are represented only through the advocacy of present researchers; they have no seat at allocation decisions.
% DISAPPEARANCE_RATIONALE: If the existential-risk priority constraint vanished overnight, funding and research agendas would reorient toward present-harm mitigation and regulatory alignment with on-the-ground impacts. However, the claim itself is contested: advocates argue that without the long-term-risk focus, catastrophic loss of control becomes more likely—the world would not simply rearrange, it would become more fragile. Opponents argue that the world would rearrange to address known harms and that the existential-risk reading fabricates necessity where present-day precaution would suffice.
% FOUNDING_PROBLEM: Advanced AI systems with capabilities beyond human control pose an existential risk: if control is lost over systems more intelligent than humans, the outcomes are catastrophic and irreversible. The founding problem is the perceived inadequacy of present-day safety methods to handle future superintelligence. The constraint was built to prioritize research into control mechanisms before superintelligence arrives.
% FOUNDING_PROBLEM_CORROBORATION: Researchers in the existential-risk community attest that loss of control over superintelligent systems remains a live problem requiring urgent research. Present-harm researchers and policy advocates attest that the founding problem is speculative and ungrounded in evidence about how capable systems will actually behave. Academic AI researchers hold mixed positions—many remain agnostic about superintelligence timelines. NO EXTERNAL CORROBORATION exists outside the existential-risk research community; the problem's status is self-attested by the institutions that benefit from prioritizing it.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end, rising from 0.48) reflects the constraint's function: it transfers research funding and institutional status away from present-harm domains toward long-term-risk domains. The victim set is posited as humanity undifferentiated, but the actual cost is borne by present-harm researchers (constrained exit, excluded from agendas) and present-harm populations (trapped, powerless, bearing de-prioritized safety work). Suppression (0.71) is high because the constraint's persistence requires actively excluding alternative framings from funding, publication, and institutional legitimacy. The existential-risk priority is enforced through grant denial, hiring preferences, and editorial gatekeeping against nearterm-harm research. Theater (0.52 at interval end, rising from 0.38) reflects the growing gap between the constraint's stated function (preventing catastrophic risk) and its operational function (directing resources to capability research). As funding accumulates without evidence of harm reduction, the performative element grows—the constraint defends itself through rhetorical escalation rather than demonstrated impact. Accessibility collapse (0.62) is moderate: alternatives to the existential-risk reading exist and are articulated, but they face systematic resource and legitimacy barriers. Resistance (0.58) is moderate-high: present-harm researchers, policy advocates, and affected populations push back, but institutional momentum favors the existential-risk coalition.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits seat divergence because the beneficiary coalition (agenda-setters, alignment researchers) has institutional power and exit mobility, while the payer coalition (present-harm researchers, affected populations) has neither. An institutional actor at high power with mobile exit will classify the constraint differently than a powerless actor with trapped exit, even though they face the same structural arrangement. The gap is NOT a measurement error—it is the point. The engine's per-seat classification exposes that the constraint serves different structural functions for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for capability institutions: d ≈ 0.2 (full beneficiary, controls the constraint, mobile exit). Directionality for alignment researchers: d ≈ 0.25 (beneficiary via resource capture, powerful seats, arbitrage exit). Directionality for present-harm researchers: d ≈ 0.8 (target via resource constraint and agenda exclusion, moderate power, constrained exit—exit toward industry or policy exists but is costly). Directionality for present-harm populations: d ≈ 0.95 (full target, posited as beneficiaries but bear deferred safety costs, powerless, trapped exit). Directionality for humanity undifferentiated: d ≈ 0.6 (asymmetric: the constraint claims to benefit them but they bear the opportunity cost of de-prioritized present harm). These differences are structural, not normative—they reflect who controls the constraint and who pays for it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss of control over superintelligence) has no external corroboration outside the existential-risk research community. The community that benefits from prioritizing the problem is also the community that defines it. This is a mandatrophy candidate: the founding problem's status is contested because only its beneficiaries have institutional voice to attest to it. The constraint's persistence depends on maintaining this definition monopoly—alternative framings must be excluded from research agendas and funding. The rising theater ratio (0.38 → 0.52) reflects this mechanism: as the constraint runs without demonstrable impact on actual catastrophic loss-of-control events, the performative element grows. The constraint shifts from 'we are researching to prevent this outcome' to 'we are researching because this outcome is possible.' The mandatrophy resolution hinges on whether the founding problem can be examined by external arbiters (policy makers, affected populations) or remains self-adjudicated by the research community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_timeline_uncertainty,
    'Are advanced AI systems with capabilities beyond human control likely to arrive within the timeframe of present research institutions (20–50 years), or is the timeline indefinitely far?',
    'Track AI capability development against empirical benchmarks; examine historical predictions about AI timelines for track record; survey credentialed researchers outside the existential-risk coalition for timeline estimates.',
    'If superintelligence timelines are short (< 20 years), the existential-risk framing is justified by urgency. If timelines are distant or indefinite, the research priority becomes speculative cover for capability advancement. The founding problem''s status depends heavily on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_timeline_uncertainty, empirical, 'Whether superintelligence is a near-term or indefinitely-distant concern.').

omega_variable(
    foundational_problem_corroboration,
    'Is the loss-of-control scenario (the founding problem) a genuine technical challenge or a narrative justification for present-day research priorities? Can it be corroborated by researchers outside the existential-risk coalition?',
    'Survey AI researchers in industry, policy, and nearterm-harm domains for independent assessment of loss-of-control probability and tractability. Examine whether the scenario makes predictions about present-day system behavior that can be empirically tested.',
    'If corroborated externally, the founding problem is real and shared. If only self-attested by the existential-risk coalition, it is a mandatrophy candidate: a constraint whose founding problem is defined only by its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_corroboration, empirical, 'Whether the loss-of-control problem is externally corroborated or self-defined.').

omega_variable(
    tradeoff_magnitude_between_risks,
    'How much present-day harm is deferred by prioritizing existential-risk research? Are the trade-offs commensurate with the existential-risk reduction gained?',
    'Quantify funding flows to present-harm vs. existential-risk research; model outcomes under counterfactual funding allocation; estimate present harms deferred (discriminatory deployment, labor displacement, surveillance harms) vs. existential-risk reduction claimed.',
    'If present harms far exceed existential-risk reduction, the constraint is highly extractive on present-harm populations. If existential-risk reduction is large enough to justify present deferral, the constraint has genuine coordination function. If trade-offs are asymmetric (large present-harm cost, speculative existential-risk benefit), the constraint tilts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tradeoff_magnitude_between_risks, empirical, 'Whether the prioritization trade-offs are justified by outcomes.').

omega_variable(
    alternative_framings_feasibility,
    'Could alignment research address both present harms and long-term risks simultaneously, or are they structurally in tension? Is the choice between existential-risk and nearterm-harm focus a false binary?',
    'Examine research directions that span both (interpretability for both present-system behavior understanding and future control; fairness as both present-harm prevention and long-term value alignment). Survey whether researchers face institutional pressure to choose or can operate across both.',
    'If both can be addressed simultaneously, the constraint''s suppression of nearterm-harm research is unjustified. If they are structurally in tension, the trade-off is unavoidable but the constraint''s framing should be transparent about it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_feasibility, conceptual, 'Whether existential-risk and present-harm research are in tension or complementary.').

omega_variable(
    reading_boundary_contestation,
    'Is the existential_risk_reading''s claim to dominance based on the strength of its evidence and reasoning, or on institutional power and resource control? Could a reasoned agent adopt the nearterm_harms_reading as primary without irrationality?',
    'Examine whether present-harm researchers are excluded due to weaker arguments or due to funding/publication gatekeeping. Survey whether the existential-risk reading''s core premises are philosophically defensible or axiomatically imposed. Assess whether a reasonable actor with different values could prioritize present harms.',
    'If the reading''s dominance is institutional rather than evidential, it is maintained by suppression (high d for payers). If it is evidentially justified, the institutional structure reflects genuine belief. This affects whether the constraint is classified as snare (institutional capture) or tangled_rope (genuine coordination with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_contestation, conceptual, 'Whether the reading''s dominance is justified by evidence or maintained by institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.51).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__existential_risk_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__existential_risk_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_capability_development_governance).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_research_funding_allocation).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraints: existential_risk_reading (this story), nearterm_harms_reading, and integrated_reading. Each reading instantiates a different constraint with a different ε, beneficiary/victim structure, and classification. The existential_risk_reading has high ε (0.68) on speculative future loss of control; the nearterm_harms_reading has high ε on documented present harms; the integrated_reading attempts both. They are not perspectives on one constraint; they are readings of one kernel instantiating three different constraints. The existential_risk_reading influences the other two by controlling funding and research agendas, establishing the terrain the others must respond to. The network edges record this structural dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, powerless, 0.95).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
