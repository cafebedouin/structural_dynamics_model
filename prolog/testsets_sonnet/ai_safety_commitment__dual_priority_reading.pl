% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Claims
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   As the AI safety field matured, competing camps emerged: researchers
 *   focused on catastrophic misalignment risk from advanced systems, and
 *   advocates focused on documented harms from currently deployed systems
 *   (algorithmic bias, labor exploitation in content moderation,
 *   misinformation). Rather than resolve this into a single priority, a broad
 *   coalition of labs, safety institutes, and policy bridges adopted a
 *   'both/and' framing — asserting the two concerns are non-competing and
 *   mutually reinforcing. This framing performs real coordination work
 *   (preventing an internally destructive framing war) but obscures an
 *   unavoidable allocation problem: institutional budgets, legislative
 *   attention, and lab engineering hours are finite, and 'both matter
 *   equally' does not specify the split. In practice, better-funded and more
 *   dramatically framed existential-risk work has often captured
 *   disproportionate growth, while near-term harm remediation lags — even as
 *   both camps continue to publicly endorse the unifying frame because
 *   breaking coalition costs each camp legitimacy.
 *
 * KEY AGENTS:
 *   - large_frontier_labs: agenda_setter/beneficiary (institutional/arbitrage) — sets dual framing, controls actual resource splits
 *   - ai_safety_institutes: beneficiary/agenda_setter (institutional/constrained) — needs broad mandate legitimacy
 *   - near_term_harm_advocacy_groups: payer (moderate/constrained) — loses allocation share under the shared frame
 *   - algorithmically_marginalized_communities: payer (powerless/trapped) — bears present-day cost of diverted attention
 *   - existential_risk_researchers_competing_for_funding: payer/beneficiary (moderate/constrained) — competes within the same coalition for the same funding pool
 *   - legislators_and_regulators: observer (institutional/analytical) — must adjudicate the split without a clear allocation principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.48).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.35).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Claims").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c').
narrative_ontology:cs_kernel_codification('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', distributed).
narrative_ontology:cs_authority_grounding('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', distributed).
narrative_ontology:cs_reading_relation('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', foundational, priorities_are_non_competing_in_principle).
narrative_ontology:cs_axiom_status(priorities_are_non_competing_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', priorities_are_non_competing_in_principle, instrumental).
narrative_ontology:cs_axiom('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', secondary, coalition_breadth_serves_field_legitimacy).
narrative_ontology:cs_axiom_status(coalition_breadth_serves_field_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', coalition_breadth_serves_field_legitimacy, conventional).
narrative_ontology:cs_reference_frame('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', unified_ai_safety_field_pre_fracture).
narrative_ontology:cs_drift_state('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', post_funding_tilt_disclosure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f77b8ff9-dd74-415c-b66e-0dcd9b5a5f3c', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, large_frontier_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_institutes).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, cross_cutting_policy_coalitions).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_advocacy_groups).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, algorithmically_marginalized_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_researchers_competing_for_funding).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, existential_risk_researchers_competing_for_funding).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, big_tent_safety_coalition_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and publicly endorse both existential-risk research divisions and near-term fairness/safety teams, framing the two as mutually reinforcing rather than in tension. This framing lets them claim broad legitimacy across regulatory audiences, control which harms get prioritized in practice, and avoid being forced to pick a side that would constrain product timelines either way.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, large_frontier_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, large_frontier_labs, beneficiary).

% Government and quasi-governmental bodies (national AI safety institutes, standard-setting consortia) adopt the dual-priority framing to build the widest possible political coalition and avoid taking a side in the resource fight. Their mandate and funding depend on being able to say they address 'all forms of AI risk.'
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_institutes, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, ai_safety_institutes, agenda_setter).

% Advocacy and academic coalitions that have built careers and institutional positions on bridging the two camps benefit from the dual-priority framing remaining the dominant narrative; it legitimizes their bridging role and the grants that fund it.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, cross_cutting_policy_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Advocates for algorithmic bias, labor exploitation, and misinformation harms find that under the dual-priority framing, funding and regulatory attention that could address documented present-day harms is diverted toward speculative long-horizon existential scenarios, since 'both matter equally' in practice often means the more dramatic, better-funded existential framing wins allocation fights. They cannot exit the coalition without losing a seat at the table.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harm_advocacy_groups, payer,
    moderate, immediate, constrained, national).

% People currently harmed by biased hiring algorithms, discriminatory risk-scoring, and exploitative content-moderation labor bear the ongoing cost of institutional attention being split with existential-risk framing that has no immediate bearing on their situation. They have no mechanism to redirect resources and no exit from systems that already govern their lives.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, algorithmically_marginalized_communities, payer,
    powerless, immediate, trapped, national).

% Researchers focused on alignment and takeover-risk scenarios benefit from the legitimacy dual-priority framing lends their work, but also compete with near-term harm work for the same finite grant pools and legislative attention, and are pressured to demonstrate 'both/and' relevance even when their technical agenda is orthogonal to near-term deployment harms.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers_competing_for_funding, payer,
    moderate, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, existential_risk_researchers_competing_for_funding, beneficiary).

% Draft AI legislation and must decide how to allocate regulatory attention and enforcement budgets between speculative catastrophic-risk provisions and concrete anti-discrimination or labor provisions; they hear the dual-priority framing from testifying experts on both sides and must adjudicate resource splits without a clear allocation principle.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, legislators_and_regulators, observer,
    institutional, biographical, analytical, national).

% Cannot participate in present resource-allocation debates but are the notional beneficiaries of existential-risk-directed effort; their interests are invoked by advocates on both sides but they have no voice of their own in how the dual-priority framing resolves allocation disputes.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_persons_at_existential_stake, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__dual_priority_reading, future_persons_at_existential_stake).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, large_frontier_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates a fractured field: without some framing that both camps matter, funders, legislators, and labs would face constant zero-sum fights that could paralyze any AI safety action at all. The dual-priority reading lets a coalition act where a forced choice might produce gridlock.
% TRANSFER_FUNCTION: Moves attention, funding, and regulatory bandwidth between two intervention populations — those harmed today by deployed systems and researchers/institutions working on speculative future catastrophic scenarios — through a shared institutional budget that is never actually large enough to fully satisfy both framings simultaneously.
% ABSENT_VOICES: Algorithmically marginalized communities experiencing concrete harm right now are rarely in the room when the 'both matter equally' framing is adopted at the institutional level; their advocates are present but structurally weaker than well-funded existential-risk research institutions and frontier labs who set the terms of the coalition.
% DISAPPEARANCE_RATIONALE: If the dual-priority framing collapsed, either near-term harms would gain resource priority (as near-term advocates would predict) or existential-risk framing would gain unchallenged dominance in elite policy circles (as existential-risk researchers might predict) — the parties disagree sharply about which population would actually benefit from the framing's disappearance, which is itself evidence the framing currently obscures a real allocation fight rather than resolving it.
% FOUNDING_PROBLEM: The AI safety field fractured into camps that risked mutually discrediting each other in front of regulators and the public — existential-risk advocates dismissed as science-fiction alarmists, near-term-harm advocates dismissed as insufficiently ambitious — threatening to produce policy paralysis or a winner-take-all framing fight that could leave one population's harms entirely unaddressed.
% FOUNDING_PROBLEM_CORROBORATION: Large frontier labs and cross-cutting policy coalitions (the framing's principal beneficiaries) attest the founding problem remains live and the dual framing is necessary coordination. Independent policy researchers outside either camp (e.g. legislative staff conducting comparative funding audits) have documented that under the shared framing, existential-risk-adjacent programs have received disproportionate funding growth relative to near-term harm remediation programs over the past several years — corroboration from outside the beneficiary set suggests the coordination function is real but the allocation problem it was meant to solve has not actually been solved, only papered over.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) and rising over the interval: the framing began as genuine coalition-preserving coordination (0.28) but has drifted as institutional funding data reveals a persistent tilt toward existential-risk-adjacent programs, meaning the 'both matter equally' claim increasingly functions as cover for an unstated priority ordering rather than a true dual commitment. Theater ratio is high and rising (0.30 to 0.52) because an increasing share of public commitments to 'balance' both priorities is rhetorical — conferences, joint statements, and mission language — while actual budget lines diverge. Suppression is comparatively low (0.35) because no party is coercively blocked from advocating for their preferred priority; the mechanism is persuasive/framing capture rather than direct coercion, which is why resistance (0.6) remains meaningfully high — near-term harm advocates actively contest the framing rather than acquiescing to it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting labs' and institutes' seats, this looks like genuine, necessary coordination — a mature field managing legitimate scope diversity. From the near-term harm advocacy and marginalized-community seats, the same structure looks like an enforced consensus that launders an actual priority ordering (tilted toward existential risk, which is more fundable and more prestigious) behind egalitarian language. The engine's per-seat computation should reflect this: agenda-setter seats compute closer to rope/tangled_rope, payer seats closer to tangled_rope/snare, given identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Large frontier labs and safety institutes sit near the beneficiary end: they set the framing, control resource allocation in practice, and gain broad legitimacy from claiming to address 'all' AI risk without being forced to specify tradeoffs. Near-term harm advocacy groups and algorithmically marginalized communities sit near the target end: they bear the real-world cost when the framing's practical effect diverts attention from documented present harms, and their exit options are constrained or trapped — communities currently affected by biased systems cannot simply wait for a resolved allocation debate. Existential-risk researchers are dual-positioned: they benefit from the framing's legitimacy while also competing within the same coalition's finite resources, making them simultaneously a beneficiary of the coordination story and a payer within its internal fights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a destructive framing war that could produce policy paralysis or one-sided dismissal of a real risk category — was and remains partly live: both existential and near-term harms are real categories with plausible claims on institutional attention. But the specific mechanism adopted (asserting non-competition without an allocation principle) has drifted from solving that problem to obscuring an unresolved and possibly unresolvable resource fight. This is not a case of mandatrophy in the classic sense (function fully dead, form persisting) — it is a case of a genuine coordination function persisting alongside a real, actively obscured extraction, which is exactly the tangled_rope signature: the framing is not fake, but it is not innocent either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allocation_principle_absence,
    'Does the dual-priority framing include any actual mechanism for dividing finite resources between existential-risk and near-term-harm interventions, or does ''non-competing priorities'' function as a rhetorical move that defers the allocation question indefinitely?',
    'Audit actual funding flows, legislative attention hours, and lab engineering headcount allocated to each intervention type over a multi-year window, compared against public commitments to balance.',
    'If no allocation mechanism exists and funding data shows persistent tilt, the dual-priority framing functions primarily as legitimacy cover for an unstated priority ordering, sharpening the tangled_rope classification toward the snare end for the near-term-harm and marginalized-community seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_principle_absence, empirical, 'Whether the framing includes a real resource-division mechanism or merely defers the allocation question.').

omega_variable(
    kernel_reading_choice_disagreement,
    'Is the choice to treat existential risk and near-term harms as a single unified kernel (rather than two entirely separate policy domains) itself a genealogically motivated framing choice by actors who benefit from coalition breadth, or a genuinely correct account of overlapping technical and governance mechanisms?',
    'Trace whether the specific policy and technical interventions proposed under each priority actually share mechanisms (e.g., interpretability research serving both goals) versus being politically bundled despite technical independence.',
    'If the mechanisms are genuinely shared, the dual-priority reading has stronger coordination legitimacy; if the bundling is primarily political, the coordination claim weakens and the extraction reading (resource diversion) strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_disagreement, conceptual, 'Whether the dual-priority kernel reflects genuine technical overlap or political bundling for coalition purposes.').

omega_variable(
    future_persons_representation,
    'Given that future persons at existential stake cannot represent themselves in present resource-allocation debates, does their exclusion bias the dual-priority framing toward whichever camp is better at institutionally ''speaking for'' them — and does that advantage systematically favor existential-risk framing over near-term harm framing regardless of the merits?',
    'Compare institutional influence and funding access of organizations claiming to represent long-term/future interests versus organizations representing documented present-day affected populations.',
    'If representation asymmetry systematically favors existential-risk advocacy institutions, this partially explains the observed funding tilt independent of the actual merits of either risk category.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_persons_representation, conceptual, 'Whether structural inability of future persons to self-advocate biases the coalition toward existential-risk framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__dual_priority_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__dual_priority_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__dual_priority_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__dual_priority_reading, theater_ratio, 24, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__dual_priority_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__dual_priority_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__dual_priority_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__dual_priority_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__dual_priority_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__dual_priority_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__dual_priority_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__dual_priority_reading, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.15).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_safety_commitment kernel. existential_risk_reading and near_term_harms_reading each instantiate a single-priority claim with a narrower, non-overlapping victim/beneficiary set and their own stable ε. This dual_priority_reading is structurally distinct: its victim set is the union of both sibling readings' affected populations, plus a novel population (existential-risk researchers competing for shared funding pools) that neither sibling reading generates on its own. The three stories should never be merged or averaged — each is evaluated independently, and this story's ε (0.48) reflects the allocation-incoherence problem specific to claiming non-competition without a division mechanism, which is not present in either single-priority reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
