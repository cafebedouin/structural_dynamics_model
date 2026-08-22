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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Commitments
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   Since roughly 2021-2023, major AI labs, funders, and governance bodies
 *   have converged on public language asserting that existential risk and
 *   near-term/documented harms are 'not competing priorities' — that
 *   addressing one does not trade off against the other. This story authors
 *   that specific claim as a constraint: the institutional commitment to
 *   treat both harm categories as simultaneously fundable and simultaneously
 *   urgent, under one safety umbrella. It is one of three readings of a
 *   contested kernel about what 'AI safety' means. The sibling readings —
 *   existential_risk_reading (safety = preventing extinction-level
 *   misalignment) and near_term_harms_reading (safety = preventing documented
 *   present harms like bias, labor exploitation, misinformation) — are
 *   separate constraints with their own ε, victim sets, and stakeholder
 *   structures; they are not folded into this file. This reading's
 *   distinguishing structural feature is the union of both victim populations
 *   and the resource-allocation coherence problem that union creates under
 *   real budget scarcity.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: agenda_setter (institutional/arbitrage) — sets and administers the dual framing, controls actual resource split
 *   - algorithmically_marginalized_communities: payer (powerless/trapped) — bears present, documented harms diluted by simultaneous x-risk claims
 *   - long_horizon_safety_researchers: payer/beneficiary (moderate/identity_locked) — competes for share of unified budget, identity invested in framing
 *   - independent_policy_analysts: observer (analytical) — audits whether the non-competing claim is resourced as claimed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.52).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.38).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Commitments").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '697b1d75-fba2-4ad7-9bd3-e38cb6b141bc').
narrative_ontology:cs_kernel_codification('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', distributed).
narrative_ontology:cs_authority_grounding('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', distributed).
narrative_ontology:cs_reading_relation('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', foundational, harm_categories_are_jointly_addressable_without_tradeoff).
narrative_ontology:cs_axiom_status(harm_categories_are_jointly_addressable_without_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', harm_categories_are_jointly_addressable_without_tradeoff, empirically_contingent).
narrative_ontology:cs_axiom('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', secondary, safety_mandate_legitimacy_requires_comprehensive_scope).
narrative_ontology:cs_axiom_status(safety_mandate_legitimacy_requires_comprehensive_scope, holdable).
narrative_ontology:cs_axiom_grounding('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', safety_mandate_legitimacy_requires_comprehensive_scope, instrumental).
narrative_ontology:cs_reference_frame('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', unified_safety_mandate_pre_scarcity_framing).
narrative_ontology:cs_drift_state('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', post_2023_compute_and_funding_scarcity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('697b1d75-fba2-4ad7-9bd3-e38cb6b141bc', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_research_institutes).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, philanthropic_funders_of_x_risk).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, algorithmically_marginalized_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, content_moderation_and_data_labeling_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, long_horizon_safety_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, long_horizon_safety_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the public 'both matter' framing in governance statements, safety team charters, and funding allocations. Administers the actual split between x-risk alignment research and near-term harm mitigation teams, and can reallocate headcount and compute between them at will. Benefits from the framing because it defuses pressure from either advocacy camp simultaneously and lets the lab claim comprehensive responsibility without binding commitments to either.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive funding and legitimacy under the dual-priority umbrella, which lets existential-risk-focused researchers and near-term-harm researchers coexist in the same institutional budget lines without having to justify tradeoffs publicly. Their continued funding depends on the umbrella framing remaining unresolved rather than adjudicated.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_research_institutes, beneficiary,
    organized, generational, constrained, global).

% Fund long-horizon alignment work under a framing that lets them claim solidarity with near-term harm advocates while directing the overwhelming majority of dollars toward existential risk. The 'non-competing' language shields their allocation choices from scrutiny about opportunity cost.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, philanthropic_funders_of_x_risk, beneficiary,
    powerful, civilizational, mobile, global).

% Experience documented, present-tense harms from deployed systems: discriminatory scoring, surveillance, wage suppression, misinformation targeting. Under the dual-priority framing, resources and regulatory urgency that could address these harms directly are diluted by simultaneous claims on attention and funding for speculative long-horizon risk. They have no exit from systems already deployed against them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, algorithmically_marginalized_communities, payer,
    powerless, immediate, trapped, national).

% Perform the ongoing labor that keeps AI systems safe in the mundane sense (filtering, labeling, correcting) under labor conditions the dual-priority framing rarely names as a safety issue at all, since 'safety' rhetoric is captured by both x-risk and, to a lesser extent, algorithmic-bias framings that exclude labor conditions. Their exit is constrained by economic dependency on the only jobs the AI supply chain offers them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, content_moderation_and_data_labeling_workers, payer,
    powerless, immediate, trapped, global).

% Career and self-concept are built around existential risk as the central problem; the dual-priority framing forces them to compete for a share of a nominally unified budget with near-term harm researchers, diluting resources they believe are civilizationally decisive, while giving them enough institutional cover to avoid being labeled dismissive of present harms. Their identity investment in the field makes exit from the framing costly regardless of resource outcomes.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, long_horizon_safety_researchers, payer,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, long_horizon_safety_researchers, beneficiary).

% Are expected to legislate against both categories of harm simultaneously but receive from industry a unified 'safety' vocabulary that obscures which specific harm any given proposed rule addresses, making it harder to write targeted, enforceable regulation for either category. Their voice on how to disaggregate the categories for legislative purposes is rarely solicited by the labs setting the framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, regulators_and_policymakers, excluded,
    institutional, biographical, constrained, national).

% Study whether the dual-priority commitment produces coherent resource allocation or functions mainly as rhetorical cover, tracking dollars and headcount against public statements.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, independent_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely allows a single institution to hold both a long-horizon existential-risk research agenda and a near-term harm-mitigation agenda under one safety mandate, avoiding the need to publicly rank one population's stakes against the other's and enabling coalition-building across otherwise rival advocacy communities.
% TRANSFER_FUNCTION: Moves funding, researcher attention, regulatory bandwidth, and public legitimacy between two claimant populations — speculative future beings/humanity-at-large versus presently harmed marginalized communities and platform labor — through a single 'safety' budget and vocabulary, with the actual split set unilaterally by the institutions administering it.
% ABSENT_VOICES: Regulators seeking to write targeted rules for either harm category are not consulted on how the dual framing should be disaggregated for legislative purposes; content moderation and data labeling workers are rarely invited into either the x-risk or near-term-harm safety conversations despite bearing daily costs of the systems both camps discuss abstractly.
% DISAPPEARANCE_RATIONALE: Existential-risk researchers argue that abandoning the dual framing would strip cover from near-term harm work and force honest tradeoff debates that might starve long-horizon research; near-term harm advocates argue the framing already functions to subordinate their claims and its disappearance would force overdue reckoning with concrete, remediable harms; the labs administering it argue disappearance would fracture safety coalitions currently unified under one banner.
% FOUNDING_PROBLEM: Two normatively serious but resource-competing claims about what 'AI safety' means emerged roughly simultaneously — the risk that advanced systems could cause catastrophic or extinction-level harm, and the observed fact that deployed systems already cause discriminatory, exploitative, and epistemic harms to identifiable populations — and institutions needed a way to claim responsibility for both without publicly choosing between them.
% FOUNDING_PROBLEM_CORROBORATION: Frontier labs and x-risk funders attest the dual framing reflects a genuine, still-live need to address both risk classes simultaneously. Independent policy analysts and several near-term-harm researchers, writing outside the labs' funding structures, attest that the framing functions largely as an allocation-avoidance mechanism — its 'non-competing' claim is asserted, not demonstrated by comparable resourcing, and no neutral audit of the actual budget split has been published by any institution using the framing.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) is moderate: the coordination function is real (a single institution genuinely can and does fund both agendas), but the framing also serves to shield unequal resource splits from scrutiny — hence extraction is present but not dominant, unlike a pure snare. Suppression (0.38) is moderate-low: no one is coercively barred from advocating for either priority, but the vocabulary itself makes it harder to demand disaggregated accounting. Theater ratio (0.58, rising from 0.30) is the most diagnostic metric here: as the 'non-competing priorities' language became standard institutional boilerplate, the ratio of public commitment-statements to audited, disaggregated resource allocation has grown — this is the Goodhart signature of a coordination claim increasingly substituting rhetoric for resourcing. Accessibility collapse (0.35) is low-moderate because alternative framings (picking one priority, or demanding explicit tradeoff disclosure) remain visible and are actively argued by both advocacy camps — the framing has not foreclosed them. Resistance (0.62) is high because both x-risk researchers and near-term-harm advocates increasingly publicly contest the framing from opposite directions, which is itself evidence this is not settled coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and x-risk funders sit near the beneficiary end: the framing lets them claim comprehensive responsibility while retaining discretion over the actual split, with minimal binding accountability to either claimant population. Ai safety research institutes similarly benefit from ambiguity that keeps both funding streams flowing into one umbrella. The two payer populations — algorithmically marginalized communities and content moderation/data labeling workers — are structurally trapped: they cannot exit the deployed systems harming them, and the framing does not name their situation with resourcing proportional to its urgency. Long-horizon safety researchers are the most structurally interesting seat: they are simultaneously beneficiaries of the umbrella's legitimacy and payers within it, since the dual framing dilutes their claim on total resources relative to a world where x-risk was funded as the sole priority — their identity lock (career and worldview built on x-risk primacy) makes their exit from the framing costly even though the framing arguably costs them resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetric mislabeling errors: treating the dual-priority commitment as pure coordination (ignoring that the 'non-competing' claim conveniently forecloses scrutiny of an actual unequal split) and treating it as pure extraction (ignoring that both harm categories are genuinely real and a unified safety mandate can genuinely serve both, at least in principle). Tangled Rope captures this: coordination function (a real problem — two legitimate safety concerns need institutional homes) plus asymmetric extraction (resource allocation systematically favors whichever population has more institutional and financial power to make its claim heard) plus active enforcement (the framing is maintained through consistent institutional messaging that treats disaggregation demands as divisive rather than as legitimate accountability requests).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_priority_resource_allocation_coherence,
    'Can ''non-competing priorities'' be operationalized as an actual resource allocation rule, or does it collapse under real budget scarcity into a de facto ranking that the framing obscures rather than resolves?',
    'Independent audit comparing publicly stated commitments to both priorities against disaggregated headcount, compute, and grant-dollar allocations across major labs and funders over a multi-year window; test whether the ratio has moved toward parity or toward one category as institutional and reputational pressures shift.',
    'If audits show a stable, defensible split proportional to some articulable principle (e.g., expected harm-weighted urgency), the coordination function is real and the tangled_rope classification''s extraction component should be revised downward. If audits show persistent, unacknowledged skew with no articulated principle, the framing functions primarily as allocation-avoidance rhetoric and the extraction component is underestimated here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_priority_resource_allocation_coherence, empirical, 'Whether the non-competing claim is resourced as a real allocation rule or functions as rhetorical cover for an unacknowledged skew.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the dual-priority reading a genuine synthesis of the two sibling readings, or is it a distinct third claim that neither existential-risk advocates nor near-term-harm advocates actually endorse in practice — i.e., is ''both matter equally'' itself a position no committed party holds, existing only as institutional messaging?',
    'Survey and discourse analysis of researchers and advocates in both camps: do self-identified members of either community actually endorse resource parity, or does each camp use dual-priority language instrumentally while privately prioritizing their own claim?',
    'If no substantial constituency genuinely holds the dual-priority position as their considered view, this constraint is best understood as an institutional artifact rather than a third live reading — which would strengthen the extraction reading of the theater_ratio trend rather than weaken it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the dual-priority reading has genuine constituents or is purely an institutional messaging artifact.').

omega_variable(
    victim_population_commensurability,
    'Are existential risk to future/global populations and documented present-day algorithmic harms to specific marginalized communities commensurable enough to be traded off against each other within a single allocation framework, or is the demand for a unified ''safety'' resource pool itself a category error?',
    'Philosophical and policy analysis of whether expected-value frameworks that aggregate speculative future harm with certain present harm produce actionable, legitimate allocation guidance, versus whether the two harm types require categorically separate governance structures with separate resourcing streams.',
    'If commensurable, the dual-priority framing''s coherence challenge is a solvable resource-allocation problem and the tangled_rope''s coordination function is stronger than currently authored. If not commensurable, the framing''s claim of ''non-competing priorities'' is definitionally false under scarcity, and this constraint drifts toward snare as the union-of-victims structure surfaces a forced, obscured tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_population_commensurability, conceptual, 'Whether existential and near-term harm claims can be meaningfully weighed within one allocation framework at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__dual_priority_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__dual_priority_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__dual_priority_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__dual_priority_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__dual_priority_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__dual_priority_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__dual_priority_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__dual_priority_reading, base_extractiveness, 24, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_safety_commitment__dual_priority_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dual_priority_reading in a three-member kernel family (ai_safety_commitment). existential_risk_reading treats extinction-level misalignment as the sole legitimate safety referent (Mountain-leaning claim of civilizational stakes, contested empirically). near_term_harms_reading treats documented present-day harms as the sole legitimate referent (Snare/Tangled-Rope-leaning, concrete identifiable victims, high resistance from affected communities). This reading synthesizes both victim sets and inherits both readings' contested elements plus a novel allocation-coherence problem neither sibling individually faces. Changes to funding patterns or public rhetoric in either sibling reading should be expected to propagate resource and legitimacy pressure into this reading's theater_ratio and extractiveness trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
