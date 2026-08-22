% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: Existential Risk Reading of AI Safety Commitment
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The existential risk reading of 'AI safety' frames the field's purpose as
 *   preventing extinction from misaligned superintelligence. This reading
 *   instantiated a massive coordination structure: billions in philanthropic
 *   funding, new research fields (alignment, interpretability, governance),
 *   and policy proposals (compute governance, licensing, pause advocacy). The
 *   coordination function is genuine — alignment is a real collective action
 *   problem. But the extraction is asymmetric: resources flow from
 *   present-day harmed communities and redirected researchers toward
 *   speculative technical bets that may not work, while frontier labs capture
 *   regulatory goodwill. The victim set includes all future humans (trapped,
 *   universal scope) and present stakeholders excluded from the framing.
 *   Theater ratio has risen as safety commitments become performative
 *   insulation for labs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.45).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "Existential Risk Reading of AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, 'c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee').
narrative_ontology:cs_kernel_codification('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', distributed).
narrative_ontology:cs_authority_grounding('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', extraction).
narrative_ontology:cs_reading_relation('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', ai_safety_commitment__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', foundational, extinction_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(extinction_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', extinction_risk_dominates_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', foundational, alignment_requires_massive_frontloaded_investment).
narrative_ontology:cs_axiom_status(alignment_requires_massive_frontloaded_investment, holdable).
narrative_ontology:cs_axiom_grounding('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', alignment_requires_massive_frontloaded_investment, instrumental).
narrative_ontology:cs_reference_frame('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', pre_deep_learning_ai_risk_theory).
narrative_ontology:cs_drift_state('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', post_chatgpt_policy_window, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c59e7bd8-8e22-4ea3-9ba4-4fc644eeaeee', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, ai_safety_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, longtermist_philanthropies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_with_safety_teams).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, all_future_humans_potentially_infinite).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_ai_researchers_redirected_to_speculative_work).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_harm_affected_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, global_south_ai_development_excluded_by_pause_proposals).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, alignment_problem_is_solvable_before_superintelligence).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, extinction_risk_dominates_expected_value_calculus).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, technical_alignment_approaches_require_massive_front_loaded_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All future human lives contingent on successful alignment of superintelligent systems. No individual can exit this stakes structure; the beneficiary status is conditional on a technical outcome not yet achieved. The extraction they bear is the opportunity cost of resources directed to speculative alignment work instead of present needs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, humanity_conditional_on_alignment_success, beneficiary,
    powerless, civilizational, trapped, universal).

% Researchers and institutions funded to work on alignment, interpretability, and governance. They benefit from expanded funding, career structures, and field legitimacy created by the existential risk framing. Exit means leaving the field entirely; professional identity is fused with the framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_safety_research_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, ai_safety_research_community, agenda_setter).

% Major funders (Open Philanthropy, Survival and Flourishing Fund, etc.) who direct capital to existential risk mitigation. They benefit from field-building leverage and narrative control over AI safety priorities. Can redirect funds to other cause areas but have committed institutional identity to longtermism.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, longtermist_philanthropies, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, longtermist_philanthropies, agenda_setter).

% OpenAI, Anthropic, DeepMind, etc. They set the technical agenda for alignment research and capture regulatory goodwill through voluntary safety commitments. Benefit from framing that legitimizes their leading position while deferring binding regulation. Exit would mean abandoning the safety framing that insulates them from stronger oversight.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_with_safety_teams, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_ai_labs_with_safety_teams, beneficiary).

% The nominal ultimate beneficiaries of successful alignment, but in the present they bear the extraction of resources diverted from immediate welfare to speculative future insurance. No voice, no exit, no representation in current decisions. Their victimization is structural: the framing treats their existence as the justification for present extraction.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, all_future_humans_potentially_infinite, payer,
    powerless, civilizational, trapped, universal).

% ML researchers pressured to pivot from capabilities or applications work to alignment/interpretability/RLHF. Career capital and funding increasingly conditional on existential risk framing. Exit means leaving AI research or accepting lower-status/ lower-funded positions outside the safety umbrella.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_ai_researchers_redirected_to_speculative_work, payer,
    moderate, biographical, constrained, global).

% Communities experiencing algorithmic bias, discrimination, labor displacement, and misinformation today. Their harms are deprioritized as 'not existential' and resources flow to speculative future work. Excluded from agenda-setting tables where AI safety priorities are negotiated.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_affected_communities, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, near_term_harm_affected_communities, excluded).

% AI researchers and institutions in Global South who would be disproportionately affected by compute governance, pause proposals, or licensing regimes designed by Northern labs. Their development trajectories are constrained by safety governance they had no role in shaping. Exit options limited by compute access and regulatory asymmetries.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, global_south_ai_development_excluded_by_pause_proposals, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, global_south_ai_development_excluded_by_pause_proposals, excluded).

% Analysts tracking the field's evolution, funding flows, and policy outcomes. They see the full structure: how the existential risk framing coordinates massive resource allocation while extracting from present-day stakeholders. No direct stake in the extraction.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_governance_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates massive resource allocation (funding, talent, compute, policy attention) toward technical alignment research and governance interventions before superintelligence emerges, solving a genuine collective action problem: no single actor can bear the cost of alignment alone, and free-riding would be rational without coordination.
% TRANSFER_FUNCTION: Moves billions in philanthropic and public funding, top-tier research talent, and regulatory bandwidth from near-term AI applications, bias mitigation, and Global South capacity building toward speculative alignment techniques (RLHF, interpretability, scalable oversight) and governance proposals (compute caps, licensing, pause advocacy).
% ABSENT_VOICES: Affected communities experiencing algorithmic harm today (marginalized groups targeted by biased systems, workers displaced by automation, Global South developers excluded by compute governance) are structurally absent from the tables where 'AI safety' priorities are set. They would object to the resource allocation but are not in the room.
% DISAPPEARANCE_RATIONALE: If the existential risk reading vanished overnight, billions in funding would redirect to near-term AI governance and applications; talent would flow back to capabilities and product work; regulatory proposals for compute caps and licensing would lose their primary justification; the field of 'AI safety' as currently constituted would dissolve or radically restructure.
% FOUNDING_PROBLEM: The alignment problem as formulated by Yudkowsky, Bostrom, and early LessWrong/MIRI community: superintelligent systems may pursue goals misaligned with human values, leading to existential catastrophe; technical solutions must be found before capabilities cross a threshold.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the originating community (MIRI, early rationalists) and longtermist philanthropies. It is contested by AI ethics researchers (Gebru, Buolamwini, Noble), Global South AI advocates, and some ML researchers (LeCun, Ng) who argue the problem is ill-posed or that near-term harms are the real problem. No neutral arbiter exists; the contest is structural.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint diverts resources from documented present harms to speculative interventions with unproven tractability. Suppression (0.45) is moderate: the framing doesn't legally forbid near-term work but structurally starves it of funding and legitimacy. Theater ratio (0.52) exceeds 0.5, indicating performative maintenance — labs adopt safety frameworks that don't constrain their core business. Accessibility collapse (0.35) is low: alternatives (near-term harm reduction, democratic AI governance) exist and are advocated. Resistance (0.28) is low but growing as affected communities organize.
 *
 * PERSPECTIVAL GAP:
 *   From the longtermist/philanthropy seat, this is a Rope: genuine coordination solving a civilization-scale problem. From the near-term harm communities and Global South developers, it's a Snare: extraction justified by a speculative future that suppresses present alternatives. From frontier labs, it's a Tangled Rope: they coordinate on safety while extracting regulatory capture. The engine will compute different types per seat — this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanity_conditional_on_alignment_success is the ultimate beneficiary but trapped — d near 1.0 (full target) despite beneficiary role because exit is impossible and benefits are conditional on unachieved outcome. AI safety researchers and longtermist philanthropies are beneficiaries with constrained/arbitrage exit — d ~0.2-0.3. Frontier labs are agenda_setters with constrained exit — d ~0.25 (they benefit but face some accountability). All victim groups are payers with trapped/constrained exit — d ~0.7-0.9. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (alignment before superintelligence) remains contested — not clearly live (timelines uncertain, tractability unproven) nor dead (risk not falsified). Mandatrophy is unresolved: if superintelligence never arrives or alignment proves intractable, the constraint becomes a Piton (inertial maintenance of a field that lost its function). If alignment succeeds, it was a Scaffold (transitional coordination). The contested status is the honest read.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_tractability_before_superintelligence,
    'Is the alignment problem technically tractable before superintelligence emerges, or is the required investment a speculative bet with no guaranteed payoff?',
    'Empirical progress on core alignment subproblems (scalable oversight, interpretability, value learning) measured against capability advances. If capabilities consistently outpace alignment, tractability is falsified.',
    'If intractable, the constraint''s extractiveness is pure waste (Snare/Piton). If tractable, extraction is the price of coordination (Tangled Rope/Rope). Determines whether ε measures necessary coordination cost or extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alignment_tractability_before_superintelligence, empirical, 'Whether technical alignment progress can keep pace with capabilities').

omega_variable(
    existential_risk_probability_estimate,
    'What is the actual probability of extinction-level outcomes from misaligned superintelligence, and how sensitive is the expected-value calculus to this estimate?',
    'Formal elicitation of expert forecasts, track record of past AI risk predictions, sensitivity analysis of EV calculations to probability ranges.',
    'If probability is very low (<0.1%), the expected value dominance collapses and the coordination function weakens (extraction becomes harder to justify). If probability is high (>10%), the coordination function strengthens. Current estimates span orders of magnitude.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability_estimate, empirical, 'Probability of extinction from misaligned superintelligence').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the existential risk reading logically foreclose the near-term harms reading within a single commitment framework, or do they coexist as competing resource claims?',
    'Analyze whether a single institutional mandate can simultaneously prioritize extinction prevention (requiring massive front-loaded speculative investment) and near-term harm reduction (requiring distributed, contextual, present-tense interventions) without resource competition.',
    'If forecloses, the kernel is structurally fractured — adopting one reading expels the other. If coexists_with, the kernel holds a persistent contest. If influences, the existential risk reading''s resource capture structurally disadvantages the near-term reading without logical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between existential risk and near-term harms readings of the AI safety kernel').

omega_variable(
    future_generations_as_victims_paradox,
    'Can ''all future humans'' be meaningfully classified as victims of a constraint that claims to benefit them, when their victimization consists of the opportunity cost of resources spent on their behalf?',
    'Intergenerational ethics analysis: does a transfer from present to future (via speculative insurance) constitute extraction from the future beneficiaries when the transfer reduces present welfare that would otherwise propagate forward?',
    'If yes, the beneficiary/victim structure is paradoxical — the same group is both. This would challenge the Tangled Rope classification which requires distinct coordinated and extracted parties. May indicate a category error in the ε-invariance decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_as_victims_paradox, conceptual, 'Whether future generations can be both beneficiaries and victims of the same constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2015, ai_safety_commitment__existential_risk_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ai_s_tr_t2018, ai_safety_commitment__existential_risk_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__existential_risk_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__existential_risk_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(ai_s_tr_t2024, ai_safety_commitment__existential_risk_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement(ai_s_tr_t2026, ai_safety_commitment__existential_risk_reading, theater_ratio, 2026, 0.5).
narrative_ontology:measurement(ai_s_tr_t2028, ai_safety_commitment__existential_risk_reading, theater_ratio, 2028, 0.51).
narrative_ontology:measurement(ai_s_tr_t2030, ai_safety_commitment__existential_risk_reading, theater_ratio, 2030, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2015, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(ai_s_be_t2018, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(ai_s_be_t2024, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(ai_s_be_t2026, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement(ai_s_be_t2028, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2028, 0.67).
narrative_ontology:measurement(ai_s_be_t2030, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2015, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(ai_s_su_t2018, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2022, 0.38).
narrative_ontology:measurement(ai_s_su_t2024, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement(ai_s_su_t2026, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2026, 0.44).
narrative_ontology:measurement(ai_s_su_t2028, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2028, 0.45).
narrative_ontology:measurement(ai_s_su_t2030, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2030, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__existential_risk_reading, 0.15).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, compute_governance_proposals).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_interpretability_funding).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, frontier_ai_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_safety_commitment kernel. The existential_risk_reading (this story) has high ε on speculative alignment work and treats future humans as conditional beneficiaries. The near_term_harms_reading has high ε on present algorithmic accountability and treats currently harmed communities as primary victims. The dual_priority_reading attempts to coordinate both but may lack enforcement mechanism. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, powerless, 0.95).
constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
