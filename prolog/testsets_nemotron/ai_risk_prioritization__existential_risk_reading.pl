% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential AI Risk Prioritization Frame
 *   domain: technology_governance/ai_safety/risk_assessment
 *
 * SUMMARY:
 *   The existential risk reading of AI risk prioritization frames misaligned
 *   AGI as an extinction-level threat requiring urgent alignment research.
 *   This reading emerged from early AI safety communities (MIRI, LessWrong,
 *   FHI) and has captured the dominant philanthropic and policy discourse
 *   through longtermist funders and frontier lab partnerships. The constraint
 *   operates as a tangled rope: it coordinates genuine research effort on a
 *   real coordination problem (how to align systems smarter than humans)
 *   while simultaneously extracting resources from near-term justice work and
 *   suppressing alternative frames through institutional gatekeeping. The
 *   victim set includes both currently harmed communities (whose issues are
 *   framed as distractions) and the postulated future humanity (whose
 *   nonexistent voices authorize the frame's moral authority).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.62).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.58).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential AI Risk Prioritization Frame").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology_governance/ai_safety/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '9c6acae6-c87b-466e-a531-25c3a3ef279b').
narrative_ontology:cs_kernel_codification('9c6acae6-c87b-466e-a531-25c3a3ef279b', distributed).
narrative_ontology:cs_authority_grounding('9c6acae6-c87b-466e-a531-25c3a3ef279b', distributed).
narrative_ontology:cs_reading_relation('9c6acae6-c87b-466e-a531-25c3a3ef279b', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('9c6acae6-c87b-466e-a531-25c3a3ef279b', foundational, extinction_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(extinction_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('9c6acae6-c87b-466e-a531-25c3a3ef279b', extinction_risk_dominates_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('9c6acae6-c87b-466e-a531-25c3a3ef279b', foundational, alignment_research_is_necessary_and_sufficient_for_existential_safety).
narrative_ontology:cs_axiom_status(alignment_research_is_necessary_and_sufficient_for_existential_safety, holdable).
narrative_ontology:cs_axiom_grounding('9c6acae6-c87b-466e-a531-25c3a3ef279b', alignment_research_is_necessary_and_sufficient_for_existential_safety, empirically_contingent).
narrative_ontology:cs_axiom('9c6acae6-c87b-466e-a531-25c3a3ef279b', secondary, near_term_harms_are_subproblems_of_alignment).
narrative_ontology:cs_axiom_status(near_term_harms_are_subproblems_of_alignment, holdable).
narrative_ontology:cs_axiom_grounding('9c6acae6-c87b-466e-a531-25c3a3ef279b', near_term_harms_are_subproblems_of_alignment, instrumental).
narrative_ontology:cs_reference_frame('9c6acae6-c87b-466e-a531-25c3a3ef279b', pre_agi_alignment_insurance_framework).
narrative_ontology:cs_drift_state('9c6acae6-c87b-466e-a531-25c3a3ef279b', post_chatgpt_frontier_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c6acae6-c87b-466e-a531-25c3a3ef279b', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, alignment_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_justice_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmic_fairness_practitioners).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, affected_communities_current_harms).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity_nonexistent_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, capability_researchers_frontier_labs).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, alignment_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive concentrated funding, talent, and institutional legitimacy from the existential risk frame. Build research agendas around alignment theory, interpretability, and scalable oversight. Career paths and organizational survival depend on this frame remaining dominant in AI governance discourse.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% Direct billions in philanthropic capital toward x-risk research (OpenPhil, FTX Future Fund legacy, SFF, etc.). The existential frame justifies their portfolio allocation and grants them agenda-setting influence over the AI safety field. Can reallocate across cause areas but have committed reputationally to longtermism.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Gain research funding and prestige but face intense publish-or-perish pressure on alignment benchmarks. Career capital is locked into the x-risk paradigm — pivoting to near-term work incurs status loss and funding uncertainty. Some experience the frame as constraining rather than enabling.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, alignment_researchers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, alignment_researchers, payer).

% Work on algorithmic discrimination, labor displacement, surveillance, and environmental impacts of deployed systems. Experience the x-risk frame as actively diverting attention and resources from harms occurring now. Funding applications are rejected for 'insufficient existential relevance'; policy windows close while x-risk narratives dominate hearings.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_justice_advocates, payer,
    organized, immediate, trapped, global).

% Build technical interventions for bias mitigation, transparency, and accountability in production systems. Their work is framed as 'safety-washing' or 'distraction' by x-risk proponents. Industry roles increasingly require alignment framing for advancement; justice-oriented research is marginalized in top venues.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmic_fairness_practitioners, payer,
    moderate, biographical, constrained, global).

% Communities experiencing algorithmic discrimination in hiring, lending, policing, healthcare, and housing today. Their harms are measurable and ongoing. The existential frame renders their suffering invisible in resource allocation — they cannot exit the systems harming them, and the discourse that could help them is starved of oxygen.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, affected_communities_current_harms, payer,
    powerless, immediate, trapped, global).

% The postulated victims of misaligned AGI extinction — all future persons who would never exist if alignment fails. They have no voice, no agency, and no ability to contest the frame that claims to protect them. Their claimed victimhood is the structural engine of the constraint's moral authority.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity_nonexistent_persons, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity_nonexistent_persons).

% Allocate regulatory attention, hearing time, and legislative drafting capacity. Face pressure from both x-risk institutions (existential frame) and civil society (near-term harms frame). Their choices determine which constraint variant gets encoded into law — the EU AI Act's risk tiers vs. SB 1047-style compute thresholds.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_governance_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Build the systems whose risks are debated. Benefit from x-risk framing because it justifies massive compute budgets and talent acquisition ('we must build AGI first to align it'). Simultaneously shape the research agenda by controlling access to frontier models. The existential frame serves their acceleration interests.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_researchers_frontier_labs, agenda_setter,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, capability_researchers_frontier_labs, beneficiary).

% Researchers mapping the field's sociology, funding flows, and epistemic dynamics. See both frames as partial readings of a contested kernel. No institutional stake in either frame's dominance. Their exit is analytical — they can change framing without career cost.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, analytical_observers_safety_research, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research attention and capital toward preventing a hypothesized extinction event from misaligned superintelligence. Solves the collective action problem of 'who pays for insurance against a low-probability, infinite-cost catastrophe' by concentrating resources on technical alignment.
% TRANSFER_FUNCTION: Moves funding, talent, policy attention, and regulatory capacity from near-term algorithmic justice interventions (bias audits, impact assessments, regulatory sandboxes for deployed systems) toward long-horizon alignment research (interpretability, scalable oversight, reward modeling, governance of frontier development).
% ABSENT_VOICES: Communities currently harmed by deployed AI systems (algorithmic discrimination victims, displaced workers, surveilled populations) are structurally excluded from x-risk governance forums. Their representatives are not invited to alignment workshops, not funded by longtermist philanthropies, not heard in compute-governance hearings. The frame treats their harms as 'already solved' or 'distractions.'
% DISAPPEARANCE_RATIONALE: If the existential risk frame vanished overnight, billions in philanthropic funding would redirect toward near-term AI governance, regulatory agendas would shift from compute thresholds to deployment accountability, research careers would pivot from interpretability to fairness/transparency, and the moral vocabulary of AI policy would center existing harms rather than hypothetical futures. The field's institutional topology would reorganize completely.
% FOUNDING_PROBLEM: Early AI safety work (Yudkowsky, Bostrom, MIRI) identified that superintelligent systems with misspecified objectives could pursue goals catastrophically misaligned with human survival. The founding problem: how to ensure that recursively self-improving systems remain beneficial regardless of capability level. This was a genuine coordination problem — no single actor could solve alignment alone, and the cost of failure was existential.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the x-risk research community (MIRI, FHI, Anthropic alignment teams) as still live and worsening with capability advances. Near-term harm advocates (Algorithmic Justice League, Data & Society, DAIR Institute) and independent scholars (Emily Bender, Timnit Gebru, Meredith Whittaker) attest that the founding problem has been operationally displaced — current frontier systems show no recursive self-improvement, and the frame now serves to justify capabilities acceleration. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the frame's capture of the majority of AI safety funding and talent while delivering contested empirical value — no misaligned AGI has appeared, and alignment progress is measured by proxy benchmarks. Suppression (0.58) captures the active marginalization of near-term harm work: conference rejections, funding denials, policy exclusion, and discursive framing of justice work as 'safety-washing.' Theater ratio (0.38) reflects genuine alignment research activity mixed with performative safety-washing by frontier labs. Accessibility collapse (0.45) is moderate — alternatives exist (near-term governance, participatory AI) but are structurally starved. Resistance (0.52) is significant from affected communities and justice advocates but remains fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk institution seat, the constraint is genuine coordination (rope-like) — they solve a real collective action problem. From the near-term justice advocate seat, it is extraction (snare-like) — their work is actively suppressed. From the alignment researcher seat, it is tangled — they get funding but lose intellectual freedom. From the affected community seat, it is invisible — they are not in the room. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the author's assessment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions and longtermist funders are structural beneficiaries (d ~ 0.15-0.25): they collect resources, set agendas, and face mobile/arbitrage exit. Alignment researchers are dual-positioned — beneficiaries of funding but payers of career constraint (d ~ 0.45). Near-term justice advocates and affected communities are targets (d ~ 0.8-0.95): trapped, identity-locked, bearing the cost of resource diversion with no exit. Future humanity is a non-agent victim — their postulated victimhood generates the frame's moral authority but they experience nothing. Capability researchers are agenda-setters with arbitrage exit who benefit from the frame's accelerationist logic. Policymakers are constrained agenda-setters caught between frames.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recursive self-improvement alignment) remains contested — some argue it's more urgent than ever (capability curves), others that it was always speculative and current harms are the real mandate. The frame has not resolved its mandatrophy; it has expanded its mandate from 'align superintelligence' to 'govern all AI development' via compute thresholds and licensing, capturing regulatory capacity that might have addressed deployment harms. This expansion is the signature of mandatrophy: the original function (theoretical alignment) atrophied (no superintelligence exists), but the constraint grew new enforcement mechanisms (compute governance) that serve the beneficiaries' interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability,
    'What is the actual probability of misaligned AGI causing human extinction within 10-100 years, conditional on current capability trajectories?',
    'Formal elicitation of forecasters with track records, decomposition into conditional probabilities (AGI timeline, alignment difficulty, takeoff speed, offense-defense balance), and comparison to base rates of existential risk from other sources.',
    'If probability is < 0.1%, the frame''s extraction from near-term harms is disproportionate to the coordinated insurance value. If > 10%, the coordination function dominates and extraction is the price of insurance. The current 1-10% range (wide expert disagreement) makes the constraint''s type genuinely ambiguous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability, empirical, 'Whether the existential threat is probable enough to justify the frame''s resource capture.').

omega_variable(
    alignment_tractability,
    'Is the technical alignment problem tractable by current research paradigms (interpretability, scalable oversight, RLHF/RLAIF, mechanistic interpretability) before AGI deployment?',
    'Track record of alignment benchmarks vs. capability benchmarks over time; whether alignment techniques scale with capabilities or hit fundamental barriers; whether any paradigm has demonstrated generalization to qualitatively novel capabilities.',
    'If tractable, the coordination function is genuine and the frame''s resource concentration is efficient. If intractable, the frame coordinates effort toward a dead end while suppressing work that addresses tractable harms — converting tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_tractability, empirical, 'Whether the frame''s coordination target is actually reachable by its methods.').

omega_variable(
    frame_capture_vs_genuine_consensus,
    'Does the existential frame''s dominance reflect genuine expert consensus on risk priority, or institutional capture by longtermist funders and frontier labs?',
    'Sociology of funding flows: track philanthropic dollar origins, conference program committee composition, journal editorial boards, policy advisory roles, and revolving doors between labs and safety institutes. Compare to stated priorities of unaffiliated ML researchers (NeurIPS/ICML surveys).',
    'If capture, the frame is a snare masquerading as rope — beneficiaries manufacture the coordination problem they claim to solve. If genuine consensus, the frame''s suppression of alternatives reflects legitimate epistemic prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frame_capture_vs_genuine_consensus, conceptual, 'Whether the frame''s institutional dominance is earned or manufactured.').

omega_variable(
    kernel_reading_relationship,
    'Does the existential_risk_reading foreclose the near_term_harms_reading, coexist with it, or influence it structurally?',
    'Analyze whether any single governance framework can simultaneously prioritize extinction prevention and near-term justice without resource trade-offs. Track whether x-risk institutions formally oppose near-term regulation (SB 1047 vs. algorithmic accountability acts).',
    'If forecloses, the kernel admits only one reading — the other is structurally impossible. If coexists_with, both are live positions in a perpetual contest. If influences, the existential frame creates downstream pressure (funding, policy attention) that deforms the near-term frame without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between the two readings of the ai_risk_prioritization kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_risk_existential_tr_t2014, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(ai_risk_existential_tr_t2017, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement(ai_risk_existential_tr_t2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(ai_risk_existential_tr_t2022, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2022, 0.34).
narrative_ontology:measurement(ai_risk_existential_tr_t2024, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_risk_existential_be_t2014, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2014, 0.28).
narrative_ontology:measurement(ai_risk_existential_be_t2017, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2017, 0.35).
narrative_ontology:measurement(ai_risk_existential_be_t2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(ai_risk_existential_be_t2022, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(ai_risk_existential_be_t2024, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_risk_existential_su_t2014, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement(ai_risk_existential_su_t2017, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2017, 0.35).
narrative_ontology:measurement(ai_risk_existential_su_t2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(ai_risk_existential_su_t2022, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement(ai_risk_existential_su_t2024, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.22).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, frontier_ai_governance__compute_thresholds).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_safety_funding_allocation__longtermist_portfolio).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, algorithmic_accountability_regulation__deployment_focus).

% DUAL FORMULATION NOTE:
% This constraint and near_term_harms_reading form the ai_risk_prioritization constraint family. The kernel 'AI risk prioritization' decomposes into two readings with different ε values: existential reading ε ≈ 0.62 (substantial extraction from near-term work), near-term reading ε ≈ 0.35 (coordination of justice interventions with lower suppression). They are linked because the existential frame cites the kernel's urgency to justify suppressing the near-term frame, and the near-term frame cites the kernel's empirical grounding to challenge the existential frame's speculative basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, institutional, 0.2).
constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, powerful, 0.3).
constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, organized, 0.75).
constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, moderate, 0.65).
constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
