% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated Alignment Priority: Catastrophic and Present Harms as Complementary Mandates
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the 'integrated reading' of the contested AI
 *   alignment priority kernel: the claim that catastrophic (existential) and
 *   present (deployment-harm) risks are complementary rather than competing
 *   priorities, and that a single dual-track methodology (red-teaming plus
 *   audits) can serve both without structurally favoring either. The
 *   integrated frame functions as a genuine coordination device — it prevents
 *   an outright zero-sum fight over safety-team attention between two
 *   legitimate risk communities — but it is also actively enforced
 *   (governance teams control the allocation and the narrative
 *   simultaneously), and it has identifiable payers on both temporal ends:
 *   present-day marginalized groups harmed by deployed systems, and future
 *   populations exposed to under-resourced capability risk. Both victim
 *   classes are structurally underrepresented in the rooms where the actual
 *   split of resources is decided, which is what pushes this reading toward
 *   tangled_rope rather than a clean rope. This is deliberately ONE of three
 *   readings of the same kernel; the existential_risk_reading and
 *   nearterm_harms_reading are separate constraint stories with their own ε
 *   and stakeholder sets, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - frontier_lab_governance_teams: agenda_setter (institutional/arbitrage) — designs and controls the dual-track allocation
 *   - policy_coalition_brokers: beneficiary/agenda_setter (organized/mobile) — gains standing by brokering the compromise
 *   - marginalized_deployment_affected_groups: payer (powerless/trapped) — present-day harm, no allocation seat
 *   - future_populations_under_capability_risk: payer, non-agent (powerless/trapped) — cannot participate, represented only by proxy
 *   - external_governance_auditors: observer (institutional/analytical) — checks stated commitments against actual budgets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.42).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.38).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated Alignment Priority: Catastrophic and Present Harms as Complementary Mandates").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'c8dca3ec-ea8b-400c-9b16-053b15fb2b76').
narrative_ontology:cs_kernel_codification('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', distributed).
narrative_ontology:cs_authority_grounding('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', distributed).
narrative_ontology:cs_reading_relation('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', foundational, catastrophic_and_present_harms_are_jointly_addressable).
narrative_ontology:cs_axiom_status(catastrophic_and_present_harms_are_jointly_addressable, holdable).
narrative_ontology:cs_axiom_grounding('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', catastrophic_and_present_harms_are_jointly_addressable, instrumental).
narrative_ontology:cs_axiom('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', secondary, shared_methodology_prevents_zero_sum_capture).
narrative_ontology:cs_axiom_status(shared_methodology_prevents_zero_sum_capture, holdable).
narrative_ontology:cs_axiom_grounding('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', shared_methodology_prevents_zero_sum_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', pre_integration_bifurcated_safety_field).
narrative_ontology:cs_drift_state('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', contemporary_institutionalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8dca3ec-ea8b-400c-9b16-053b15fb2b76', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, frontier_lab_governance_teams).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, policy_coalition_brokers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, cross_disciplinary_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_groups).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, underfunded_nearterm_harms_researchers).
narrative_ontology:constraint_vindicates(ai_alignment_priority__integrated_reading, complementarity_of_safety_priorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the dual-track methodology — red-teaming for catastrophic capability risks alongside bias/harm audits for deployed systems — and decides how much staff time, compute, and publication bandwidth each track receives. Can rebalance the portfolio unilaterally when a funder, regulator, or reputational event pushes attention toward one track.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, frontier_lab_governance_teams, agenda_setter,
    institutional, generational, arbitrage, global).

% Builds legislative and multilateral coalitions by presenting the integrated frame as a way to avoid choosing sides between existential-risk funders and civil-rights advocates. Gains standing and funding by brokering the 'both matter' compromise; can walk away to a narrower coalition if the integration becomes politically costly.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, policy_coalition_brokers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, policy_coalition_brokers, agenda_setter).

% Occupies a research niche legitimized by the integrated frame — grants, journals, and conference tracks now exist for work that spans capability risk and deployment harm. Career security depends on the frame's continued institutional credibility.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, cross_disciplinary_safety_researchers, beneficiary,
    moderate, biographical, mobile, national).

% Experiences discriminatory or extractive harm from already-deployed systems today. Under the integrated frame, their remediation competes for the same finite compute-safety and policy-attention budget as speculative future-catastrophe work, and resourcing decisions are made by actors who do not share their exposure. Cannot exit the systems that harm them; has no seat setting the allocation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_groups, payer,
    powerless, immediate, trapped, local).

% Bears the tail risk of catastrophic loss of control if capability work is under-resourced relative to nearterm harm work. Cannot participate in any present negotiation over the allocation; represented only by proxy advocates who compete with present-harm advocates for the same seats at the table.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__integrated_reading, future_populations_under_capability_risk).

% Does the audit, bias-testing, and affected-community engagement work but competes for a shrinking slice of a budget that governance teams frame as already serving their concerns via the integrated label — even when actual dollars and headcount skew toward capability red-teaming. Can move to advocacy organizations but loses access to lab-internal levers.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, underfunded_nearterm_harms_researchers, payer,
    moderate, biographical, constrained, national).

% Would argue the integrated frame dilutes attention and resources away from the single risk that matters most if realized — irreversible loss of control. Not formally excluded from the conversation but structurally disfavored whenever the integrated frame's political appeal wins internal allocation debates; their preferred singular focus is treated as a defeated position rather than argued against directly.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_focused_funders, excluded,
    powerful, generational, mobile, global).

% Would argue the integrated frame lets labs claim credit for nearterm-harms work without matching resource commitment, using catastrophic-risk framing to justify continued deployment of harmful systems under a promise of eventual balance. Present in coalition meetings but without allocation authority.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, justice_focused_advocacy_groups, excluded,
    organized, immediate, constrained, national).

% Evaluates whether labs' actual budget and staffing allocations match their stated integrated commitments, or whether the 'complementary priorities' framing functions as rhetorical cover for whichever track is more institutionally convenient at a given moment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, external_governance_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a genuine collective-action failure: treating catastrophic and present harms as a zero-sum resource fight would let whichever advocacy coalition is politically stronger at a given moment capture the entire safety budget, starving the other track entirely. The integrated frame coordinates a shared methodology (red-teaming plus audits) and a shared institutional home so both risk classes get sustained, non-adversarial attention.
% TRANSFER_FUNCTION: Moves institutional legitimacy, funding, and staff attention between two risk-focused constituencies via a single governance apparatus; in practice, allocation decisions inside that apparatus can quietly favor one track (typically capability/catastrophic work, which aligns with lab prestige and existential-risk funder preferences) while the integrated label absorbs credit for both.
% ABSENT_VOICES: Marginalized groups harmed by deployed systems today and future populations at risk from loss of control are both structurally absent from the room where the allocation is actually negotiated — the former lack organizational power inside labs, the latter cannot be represented at all except by proxy. Existential-risk funders and justice advocates are present but without allocation authority, so their objections shape rhetoric more than budgets.
% DISAPPEARANCE_RATIONALE: If the integrated framing vanished, governance teams and policy brokers who built careers and coalitions on 'both matter' would lose their brokering function and the political cover it provides — a real rearrangement for them. Whether the underlying safety work would meaningfully change is disputed: nearterm-harms researchers argue their actual audit budgets are already thin regardless of the frame, so disappearance might just make an existing imbalance explicit rather than change it; existential-risk researchers argue the same. The frame's disappearance would mainly remove a rhetorical device, not necessarily reallocate resources.
% FOUNDING_PROBLEM: Two distinct research and advocacy communities — one focused on preventing catastrophic loss of control over advanced AI, one focused on preventing present discriminatory and extractive harms from deployed systems — were competing for the same limited pool of institutional attention, safety-team headcount, and regulatory bandwidth, with each community accusing the other of distraction. The integrated frame was built to stop that competition from becoming zero-sum.
% FOUNDING_PROBLEM_CORROBORATION: Governance teams and policy brokers (who benefit from the frame) attest the competition has been resolved into genuine complementarity. External governance auditors and several justice-focused advocacy groups — outside the beneficiary set — attest that budget and headcount data show the competition persists beneath the integrated label, with capability-risk work still receiving disproportionate resourcing; no fully disinterested third party has published a comprehensive cross-lab allocation audit, so the corroboration on the 'resolved' side remains thinner than the corroboration on the 'persists' side.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).
:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high because the integrated frame does perform real coordination work — it is not purely a cover story, unlike a pure snare. But it rises over the interval as the frame becomes institutionally entrenched and used increasingly to justify allocations that in practice tilt toward capability-risk work, which is more prestigious and more aligned with major funder interests. Suppression is moderate (0.38): dissenting voices from both the existential-risk-only and justice-focused-only camps are not silenced outright, but their objections are absorbed into 'complementary priorities' rhetoric that makes structural imbalance harder to name as imbalance. Theater ratio rises modestly (0.15 to 0.31) as the integrated label increasingly does rhetorical work that outpaces matched resourcing — the classic Goodhart signature of a coordination frame drifting toward proxy compliance ('we address both') substituting for the real balancing act.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the integrated frame looks like genuine, hard-won coordination between two legitimate concerns — a rope. From the payer seats on both temporal ends, the same structure looks like extraction: their concerns are nominally honored but their actual resourcing is subject to a discretionary allocation controlled by parties who benefit from appearing balanced without being held to a fixed split. This divergence is exactly what the tangled_rope classification is built to capture and is not evidence against the coordination function — both are true simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Governance teams and policy brokers sit near the beneficiary end: they control the framing, collect the legitimacy and funding benefits of appearing to solve the coordination problem, and can exit to narrower framings if this one becomes costly. Both victim populations sit near the full-target end but for different reasons: marginalized deployment-affected groups are trapped by circumstance (they cannot exit systems already harming them) while future populations are trapped by nonexistence (they cannot participate in any present negotiation at all) — the non-agent flag on that stakeholder reflects that it is not a present actor capable of bargaining, only a proxy-represented interest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (two safety communities fighting over the same finite institutional bandwidth) is genuinely contested as either live or dead: it has not disappeared, but the mechanism built to solve it (integration) has itself become a site of the same competition it was meant to end, now conducted through allocation opacity rather than open rivalry. This is not classic mandatrophy (mandate fully outlived, arrangement purely inertial) because the coordination function still operates — but the frame's political usefulness to governance teams and policy brokers gives it a self-perpetuating logic somewhat independent of whether it is actually resolving the underlying tension, which is the drift this story's rising theater_ratio and extractiveness series are tracking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_as_genuine_synthesis_or_deferral_mechanism,
    'Is the integrated reading a genuine third position that resolves the tension between the existential_risk_reading and nearterm_harms_reading, or is it a governance-convenient synthesis that lets institutions defer the harder resource-allocation choice that either sibling reading would force?',
    'Compare actual budget and headcount splits across labs claiming the integrated frame against labs that explicitly prioritize one sibling reading; a genuine synthesis should show comparable per-track resourcing to a lab that deliberately balances both, while a deferral mechanism should show allocation patterns indistinguishable from whichever single-priority reading the lab''s leadership actually favors.',
    'If deferral, the tangled_rope classification is conservative and the true extraction may be closer to snare-level, with the integrated label functioning primarily as legitimacy cover for a de facto single-priority allocation. If genuine synthesis, the classification should trend toward rope over time as allocation data show real balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_as_genuine_synthesis_or_deferral_mechanism, empirical, 'Whether integration is real synthesis or a cover story for continued single-priority allocation').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three kernel readings disagree — is it about the ranking of risk magnitudes (which harm is worse), about temporal discounting (how to weigh present certain harm against future uncertain catastrophe), or about institutional trust (whether the same governance apparatus can be trusted to serve both without capture by whichever constituency is currently more powerful)?',
    'Structured elicitation with proponents of each sibling reading, asking each to identify which of the three axes (magnitude ranking, temporal discounting, institutional trust) they would change their position on if given new evidence, versus which they hold as a fixed normative commitment.',
    'If the disagreement is purely about institutional trust rather than magnitude or discounting, then the integrated_reading''s core claim (complementarity) may be compatible with both siblings, and the real fight is about allocation-mechanism design, not priority ranking — which would suggest the integrated reading''s tangled_rope classification stems entirely from implementation failure, not from a flaw in the complementarity claim itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the actual axis of disagreement among the three kernel readings').

omega_variable(
    future_population_representation_validity,
    'Can any present institutional arrangement legitimately claim to represent the interests of future populations under capability risk, given that those populations cannot consent, object, or bargain?',
    'No empirical resolution is fully available; this is a standing problem in intergenerational ethics. Partial evidence could come from comparing proxy-advocacy outcomes (e.g., long-termist research allocations) against later assessments of whether the risks proxies emphasized were the risks that materialized.',
    'If proxy representation is judged illegitimate or unreliable, the integrated reading''s claim to balance present and future harms rests on an unfalsifiable proxy judgment, which weakens the coordination-function claim and pushes the classification toward the extraction end, since one victim class can never check whether it was actually served.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_population_representation_validity, conceptual, 'Whether future populations can be legitimately represented in present resource-allocation decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__integrated_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__integrated_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__integrated_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'AI alignment priority' per the ε-invariance principle. existential_risk_reading and nearterm_harms_reading are sibling readings of the same contested kernel (ai_alignment_priority), each with its own ε, beneficiary/victim structure, and classification. All three should be read together as a constraint family; none is the 'correct' single measurement of alignment priority — each names a different structural claim about what alignment work should prioritize and who bears the cost of getting the priority wrong.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
