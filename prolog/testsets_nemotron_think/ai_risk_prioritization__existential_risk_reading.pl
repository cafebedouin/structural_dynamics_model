% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential AI Risk Prioritization Framework
 *   domain: technology_governance/ai_safety/risk_assessment
 *
 * SUMMARY:
 *   The existential risk reading of AI risk prioritization frames misaligned
 *   AGI as the dominant threat to humanity's future, justifying massive
 *   resource allocation to alignment research. This reading became
 *   institutionally dominant post-2015 through concentrated funding (Open
 *   Philanthropy, Long-Term Future Fund), talent recruitment (80,000 Hours,
 *   EA movement), and venue control (NeurIPS safety workshops, ICLR,
 *   dedicated journals). The constraint operates as a prioritization
 *   framework: it coordinates genuine research effort toward a hypothesized
 *   catastrophic risk while structurally suppressing competing framings
 *   (algorithmic justice, fairness, accountability) by labeling them
 *   'near-term distractions.' The claimed type is tangled_rope — genuine
 *   coordination function (building a field that didn't exist) combined with
 *   asymmetric extraction (diverting resources from currently harmed
 *   populations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.55).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential AI Risk Prioritization Framework").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology_governance/ai_safety/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'd3804200-526f-4a84-af1d-43a32e870ecf').
narrative_ontology:cs_kernel_codification('d3804200-526f-4a84-af1d-43a32e870ecf', distributed).
narrative_ontology:cs_authority_grounding('d3804200-526f-4a84-af1d-43a32e870ecf', extraction).
narrative_ontology:cs_interpretation_layer_present('d3804200-526f-4a84-af1d-43a32e870ecf').
narrative_ontology:cs_reading_relation('d3804200-526f-4a84-af1d-43a32e870ecf', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('d3804200-526f-4a84-af1d-43a32e870ecf', foundational, extinction_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(extinction_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('d3804200-526f-4a84-af1d-43a32e870ecf', extinction_risk_dominates_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('d3804200-526f-4a84-af1d-43a32e870ecf', foundational, near_term_harms_are_rounding_error).
narrative_ontology:cs_axiom_status(near_term_harms_are_rounding_error, holdable).
narrative_ontology:cs_axiom_grounding('d3804200-526f-4a84-af1d-43a32e870ecf', near_term_harms_are_rounding_error, instrumental).
narrative_ontology:cs_reference_frame('d3804200-526f-4a84-af1d-43a32e870ecf', pre_deep_learning_ai_safety_field).
narrative_ontology:cs_drift_state('d3804200-526f-4a84-af1d-43a32e870ecf', post_2015_funding_concentration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d3804200-526f-4a84-af1d-43a32e870ecf', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, alignment_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_algorithmic_justice_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, currently_affected_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, civil_society_orgs_fighting_deployed_harms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, alignment_researchers).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, instrumental_convergence_thesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, sharp_left_turn_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set research agendas, allocate funding, define the field's priorities. Control major conferences, journals, and talent pipelines. Benefit from concentrated funding flows and institutional prestige. Can pivot to other fields if funding shifts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Direct billions toward alignment research through foundations and donor-advised funds. Their capital shapes the entire field's incentive structure. Face no accountability to currently affected populations. Can redirect capital at will.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Receive funding, status, and career capital for working on existential risk. Also bear opportunity costs: pressure to frame all work through x-risk lens, difficulty publishing near-term harm work in top venues, career risk from dissenting.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, alignment_researchers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, alignment_researchers, payer).

% Work on discrimination, displacement, surveillance from deployed systems. Compete for shrinking funding pool framed as 'distraction.' Face pressure to reframe work as relevant to alignment or leave the field. Their communities are the ones currently harmed.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_algorithmic_justice_researchers, payer,
    moderate, biographical, constrained, global).

% Experience algorithmic discrimination in hiring, lending, policing, healthcare NOW. Their harms are cited as 'low stakes' relative to extinction. No voice in research prioritization. Cannot exit the systems harming them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, currently_affected_communities, payer,
    powerless, immediate, trapped, local).

% Advocate for regulation of deployed systems. Excluded from governance forums dominated by x-risk framing. Funding diverted to alignment work. Must choose between compromising framing or losing resources.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, civil_society_orgs_fighting_deployed_harms, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, civil_society_orgs_fighting_deployed_harms, excluded).

% The postulated victim set of misaligned AGI — includes nonexistent future persons. Cannot speak, consent, or resist. Their claimed interests are represented entirely by current x-risk advocates. No mechanism for accountability if the threat model is wrong.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity).

% Track funding flows, publication patterns, regulatory capture. See both the genuine coordination value of alignment research and the extraction from near-term justice. No stake in either frame's victory.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_policy_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research talent and capital toward preventing a hypothesized extinction-level catastrophe from misaligned AGI, creating shared epistemic standards and talent pipelines that did not exist before.
% TRANSFER_FUNCTION: Moves funding, talent, publication venues, and regulatory attention from near-term algorithmic harm mitigation (discrimination, displacement, surveillance) toward long-horizon alignment research and capability control frameworks.
% ABSENT_VOICES: Currently affected communities experiencing algorithmic harm today are structurally excluded from prioritization forums. Global South voices on AI deployment harms are absent. Future humanity is invoked as a silent moral patient with no mechanism for consent or dissent.
% DISAPPEARANCE_RATIONALE: If the existential risk prioritization framework vanished overnight, billions in funding would redirect to near-term safety and justice work, conference agendas would shift, regulatory proposals would focus on deployed systems, and the career incentives for thousands of researchers would invert — the entire AI governance landscape would reorganize.
% FOUNDING_PROBLEM: Early AI safety field (2000s-2010s) identified that no one was working on the control problem for superintelligent systems. The founding problem was: 'How do we ensure that systems vastly more capable than humans remain aligned with human values?' This was genuinely neglected.
% FOUNDING_PROBLEM_CORROBORATION: X-risk proponents (Bostrom, Yudkowsky, MIRI, FHI) attest the control problem remains unsolved and urgent. Near-term harm researchers (Buolamwini, Gebru, Noble, Raji) and civil society attest the founding problem has been superseded by deployed harms — the 'control problem' framing now functions to divert resources from existing victims. Independent bibliometric analyses show the field's publication and funding distribution shifted dramatically post-2015 toward longtermist priorities.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the framework captures the majority of AI safety funding and talent while the harms it suppresses are ongoing and measurable. Suppression (0.55) is moderate — not enforced by law but by funding gates, publication norms, and career incentives. Theater ratio (0.32) reflects that alignment research produces genuine technical output, but a growing share of activity is performative (grant applications framed in x-risk language, 'safety washing' of capabilities work). Accessibility collapse (0.42) is moderate: alternative framings exist but struggle for resources. Resistance (0.61) is high: algorithmic justice movement, Global South critics, and displaced researchers actively contest the frame.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (x-risk institutions), the constraint is a rope: genuine coordination solving a real collective action problem (nobody else was working on alignment). From the payer seats (near-term justice researchers, affected communities), it is a snare: their work is defunded and delegitimized by a frame that treats their lived harms as rounding error. The engine computes this divergence from the structural data — the same constraint, different seats, different types.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk institutions and longtermist funders are structural beneficiaries (d ~0.15): they control the agenda, collect the resources, face no downside from currently affected communities. Alignment researchers are mixed (d ~0.45): benefit from funding but constrained by framing demands. Near-term justice researchers and affected communities are targets (d ~0.85): bear the extraction, have trapped/constrained exit, no voice in prioritization. Future humanity is invoked as ultimate beneficiary but is a non-agent — cannot validate or contest the claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (AGI control) remains technically unsolved, so mandatrophy is contested not resolved. However, the constraint's current operation extracts from near-term harm mitigation — a function not in the original mandate. The coordination function has expanded to capture adjacent territory (all AI safety funding, all regulatory attention) while the extraction from justice work has grown. This is mandatrophy in the extended sense: the arrangement persists and grows beyond its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of a single kernel, or are ''existential risk'' and ''near-term harms'' fundamentally different risk objects that only share the label ''AI risk''?',
    'Trace whether the two readings share a common referent (AI systems causing harm) with different weightings, or whether they invoke disjoint causal models (speculative superintelligence vs. observable deployed systems). Bibliometric analysis of citation networks and funding flows.',
    'If different risk objects, the kernel_id is a false unification — two separate constraints incorrectly forced into one frame. Each would need its own ε and stakeholder structure. The current decomposition would be validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel_id correctly identifies a single contested commitment or falsely unifies distinct risk ontologies.').

omega_variable(
    future_humanity_victim_status,
    'Does future humanity (including nonexistent persons) function as a genuine victim stakeholder, or is this a rhetorical device that shields the constraint from accountability to currently existing victims?',
    'Analyze whether any mechanism exists for future humanity to consent, dissent, or hold the constraint accountable. Compare to other domains invoking future generations (climate, nuclear waste).',
    'If rhetorical device, the victim set is fabricated — the constraint has no genuine beneficiaries among its claimed ultimate victims, only institutional beneficiaries. This would shift classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_humanity_victim_status, conceptual, 'Ontological status of future humanity as victim in this constraint.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (building alignment research field) structurally separable from the extraction function (suppressing near-term justice work), or are they inextricably linked such that the field could not exist without the suppression?',
    'Counterfactual: if near-term justice work had been funded at 2010 levels while alignment funding grew, would the alignment field still have coalesced? Examine funding zero-sum dynamics and venue competition.',
    'If separable, the extraction is contingent — a tangled_rope where coordination could exist without suppression. If inextricable, the coordination story is cover for extraction — the constraint is a snare with a coordination facade.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of near-term harm framing structural (funding gates, venue control, career incentives) or internalized (researchers self-censor, reframe work to fit x-risk narrative)?',
    'Post-exit suppression trajectory: track researchers who left x-risk-aligned institutions — do they regain ability to publish near-term work, or does the framing persist? Survey current researchers on perceived pressure.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint reshapes researcher identity and epistemic frames, not just resource flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for near-term harm researchers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2010, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(ai_r_tr_t2014, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(ai_r_tr_t2016, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement(ai_r_tr_t2026, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement(ai_r_tr_t2028, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2028, 0.32).
narrative_ontology:measurement(ai_r_tr_t2030, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2030, 0.32).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2010, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(ai_r_be_t2014, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement(ai_r_be_t2016, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2016, 0.35).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement(ai_r_be_t2026, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2026, 0.67).
narrative_ontology:measurement(ai_r_be_t2028, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2028, 0.68).
narrative_ontology:measurement(ai_r_be_t2030, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2010, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(ai_r_su_t2014, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2014, 0.2).
narrative_ontology:measurement(ai_r_su_t2016, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2016, 0.35).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2022, 0.53).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement(ai_r_su_t2026, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement(ai_r_su_t2028, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2028, 0.55).
narrative_ontology:measurement(ai_r_su_t2030, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.1).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_governance_funding_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_regulation_prioritization).

% DUAL FORMULATION NOTE:
% This constraint and near_term_harms_reading form the ai_risk_prioritization constraint family. They share the kernel 'AI risk prioritization' but instantiate different constraints with different ε, different victim sets, different timescales. The existential reading has higher ε (0.68 vs estimated 0.35 for near-term reading) because it extracts from a larger, more vulnerable population (currently affected communities) over a longer horizon with less accountability. The near-term reading has lower suppression (estimated 0.30) because its harms are observable and contestable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, powerless, 0.95).
constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
