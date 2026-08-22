% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance: Bridging (Present Harms + Existential Risk) Reading
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the bridge reading of the AI risk governance
 *   priority kernel: the claim that present harms and existential risks are
 *   non-mutually-exclusive and structurally entangled, requiring unified
 *   frameworks rather than a prioritization choice between them. This is a
 *   distinct constraint from the existential_risk_reading (which claims
 *   strict priority for catastrophic-scenario prevention) and the
 *   near_term_harms_reading (which claims strict priority for documented
 *   present harm) — those are separate files with their own ε and stakeholder
 *   structure. The bridge reading has its own coordination story (genuine
 *   cross-field synthesis was needed) and its own extraction pattern (a small
 *   set of broker institutions capture disproportionate funding and
 *   legitimacy by administering the synthesis), which is why it authors as
 *   tangled_rope with moderate ε on both the present-harm and
 *   existential-risk victim dimensions rather than as a pure rope or pure
 *   mountain claim.
 *
 * KEY AGENTS:
 *   - cross_field_broker_institutions: agenda-setting beneficiary — administers the unified framework and captures funding/legitimacy from doing so
 *   - marginalized_populations_affected_by_present_ai_harms: payer — bears diluted attention to documented present harm
 *   - future_humanity: payer — bears diluted attention to tail-risk catastrophic scenarios, has no voice
 *   - existential_risk_research_community and near_term_harms_research_community: excluded/payer — forced to adopt bridging vocabulary to retain funding access, at cost to their core priorities
 *   - ai_governance_policymakers: beneficiary — gains political cover for deferring hard prioritization choices
 *   - analytical_observers: observer — documents the concentration of cross-field brokerage in a small set of high-centrality actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.52).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.44).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance: Bridging (Present Harms + Existential Risk) Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '7b92e33a-1d3d-45d0-805f-fd3c5e099334').
narrative_ontology:cs_kernel_codification('7b92e33a-1d3d-45d0-805f-fd3c5e099334', distributed).
narrative_ontology:cs_authority_grounding('7b92e33a-1d3d-45d0-805f-fd3c5e099334', distributed).
narrative_ontology:cs_reading_relation('7b92e33a-1d3d-45d0-805f-fd3c5e099334', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('7b92e33a-1d3d-45d0-805f-fd3c5e099334', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('7b92e33a-1d3d-45d0-805f-fd3c5e099334', foundational, risk_classes_are_structurally_entangled_not_competing).
narrative_ontology:cs_axiom_status(risk_classes_are_structurally_entangled_not_competing, holdable).
narrative_ontology:cs_axiom_grounding('7b92e33a-1d3d-45d0-805f-fd3c5e099334', risk_classes_are_structurally_entangled_not_competing, empirically_contingent).
narrative_ontology:cs_axiom('7b92e33a-1d3d-45d0-805f-fd3c5e099334', secondary, unified_governance_frameworks_outperform_triage_under_scarcity).
narrative_ontology:cs_axiom_status(unified_governance_frameworks_outperform_triage_under_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('7b92e33a-1d3d-45d0-805f-fd3c5e099334', unified_governance_frameworks_outperform_triage_under_scarcity, instrumental).
narrative_ontology:cs_reference_frame('7b92e33a-1d3d-45d0-805f-fd3c5e099334', pre_bifurcation_ai_safety_discourse).
narrative_ontology:cs_drift_state('7b92e33a-1d3d-45d0-805f-fd3c5e099334', post_2023_field_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b92e33a-1d3d-45d0-805f-fd3c5e099334', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_research_centers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations_affected_by_present_ai_harms).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, ai_governance_policymakers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, existential_risk_research_community).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, near_term_harms_research_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small set of labs, centers, and standard-setting bodies produce the disproportionate share of work that cites both near-term harm literature and existential-risk literature. They administer the unified frameworks, sit on the funding panels that decide what counts as 'integrated' safety-ethics research, and translate between communities that otherwise do not read each other's journals. Their institutional position and funding depend on the bridging function continuing to be seen as necessary rather than as one contestable framing among others.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions, beneficiary).

% People subject to biased hiring algorithms, discriminatory risk-scoring, surveillance systems, and labor displacement right now. Under the bridge framework, resources and attention that could go entirely to remediating their documented harms are instead partly diverted into speculative long-horizon scenario work and joint frameworks whose near-term payoff for them is diluted by design. They have no seat in the bridging institutions that set the unified agenda and cannot exit the systems that harm them.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations_affected_by_present_ai_harms, payer,
    powerless, immediate, trapped, national).

% Non-existent yet, cannot advocate for itself, and is represented only by proxy through longtermist-adjacent researchers. Under the bridge framework, existential-risk mitigation work is diluted by mandatory integration with near-term ethics concerns, meaning resources that a pure existential-risk framing would concentrate on catastrophic-scenario prevention are instead spread across the unified agenda. If the bridge framing under-serves genuinely low-probability, high-severity tail risks in favor of tractable present-day case studies, the cost is borne entirely by generations with no representation in the process.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Researchers focused on alignment, control problems, and catastrophic misuse scenarios who argue the bridge framework forces them to justify their work in near-term-harm terms to secure funding and legitimacy, diluting attention to low-probability catastrophic scenarios. They participate in bridging venues because the alternative is marginalization, not because they endorse the equivalence of the two risk classes.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_research_community, excluded,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, existential_risk_research_community, payer).

% Researchers and advocates documenting present algorithmic discrimination, labor impacts, and surveillance harms who argue the bridge framework forces them to share funding pools, conference space, and legitimacy claims with speculative long-horizon work, diluting the urgency of documented, ongoing harm. They engage with bridging institutions because those institutions increasingly control funding calls and policy access, not because the equivalence claim reflects their priorities.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_research_community, excluded,
    organized, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, near_term_harms_research_community, payer).

% Legislators and regulators drafting AI policy who can point to the bridge framing as evidence of comprehensive, non-partisan governance that avoids taking sides between the 'doomer' and 'ethics' camps. The unified framework lets them defer hard prioritization choices by claiming to address both, which is politically convenient even when it produces vaguer, harder-to-enforce policy.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_governance_policymakers, beneficiary,
    institutional, biographical, mobile, national).

% Science-and-technology-studies researchers and meta-scientific analysts who study the citation networks and funding flows of the AI risk field, documenting that a small fraction of papers account for most cross-field linkage and that the unified framing concentrates structural power in the brokering institutions rather than distributing it.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, cross_field_broker_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem: present-harm and existential-risk research communities were talking past each other, competing for the same scarce funding and policy attention as though the two concerns were zero-sum, when many technical and institutional interventions (robustness, interpretability, accountability infrastructure, auditing regimes) plausibly serve both. The bridge framework creates shared vocabulary, joint funding calls, and cross-citation that lets insights transfer between communities that would otherwise remain siloed.
% TRANSFER_FUNCTION: Moves funding, publication venues, and policy access away from single-focus advocates (either pure near-term-harm activists or pure existential-risk researchers) toward the handful of institutions and researchers positioned to credibly claim both. It moves legitimacy from distributed, community-rooted advocacy toward centralized brokerage, and moves attention away from urgent documented harms and away from tail-risk scenario work alike, toward the intersection both communities agree is real but which is a smaller slice of either's core concern.
% ABSENT_VOICES: Neither the marginalized populations experiencing present algorithmic harm nor any representative of future humanity sits in the rooms where 'unified frameworks' are drafted; both are represented only through advocacy proxies who themselves must adopt the bridging vocabulary to be heard. The most radical members of each single-focus community — those who reject the equivalence outright — are structurally filtered out of bridging venues because participation requires accepting the premise that both concerns deserve co-equal, structurally entangled treatment.
% DISAPPEARANCE_RATIONALE: The brokering institutions and the policymakers who cite the unified framing would lose a distinct claim to comprehensiveness and legitimacy, and joint funding calls would likely split back into separately-contested near-term and existential-risk pools — a real institutional rearrangement. But both single-focus communities argue their core work would proceed largely unchanged or even accelerate without the bridging overhead, since the underlying research and advocacy do not structurally depend on the unified frame; they depend on it only insofar as current funding and publication gatekeeping has been built around it.
% FOUNDING_PROBLEM: AI risk discourse in the early-to-mid 2020s fractured into two camps that treated each other's concerns as either a distraction (existential-risk researchers accused of ignoring real, present suffering) or as insufficiently serious (near-term-harms researchers accused of missing catastrophic tail risk), producing duplicated infrastructure, competing funding asks to the same donors and legislators, and policy incoherence when both camps lobbied the same bodies with contradictory priority claims.
% FOUNDING_PROBLEM_CORROBORATION: Science-and-technology-studies researchers external to both camps (the analytical_observers seat) corroborate that the fracture and duplicated-effort problem was real, citing citation-network and funding-flow data. However, they also find that the 'solution' concentrates disproportionate structural power in a small set of bridging institutions rather than resolving the fracture through distributed collaboration, meaning the founding problem may be substantially restated rather than resolved — the coordination failure has arguably been replaced by a new, narrower gatekeeping structure rather than dissolved.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate and rising: the coordination function (breaking down siloed research communities) is real and produces genuine value at low interval start, but over time an increasing share of funding and legitimacy flow specifically to brokering institutions rather than to distributed research in either camp, consistent with the citation-network finding that ~5% of papers account for ~85% of cross-field links — a small set of gatekeepers capturing outsized structural position. Suppression (0.44) is moderate: single-focus researchers are not banned from working outside the bridge frame, but funding calls, prestigious venues, and policy access increasingly require adopting the unified vocabulary, constraining rather than eliminating alternatives. Theater ratio (0.4) reflects that a meaningful fraction of 'integrated' framework activity is citation-signaling and cross-field name-checking rather than substantive joint technical work — a partial but real theatrical component. Accessibility collapse (0.38) is moderate-low: real alternatives to the bridge framing persist in both single-focus communities, they are just increasingly disadvantaged in resource competition, not eliminated.
 *
 * DIRECTIONALITY LOGIC:
 *   Broker institutions and policymakers sit near the beneficiary end: brokers gain funding and legitimacy from administering the synthesis; policymakers gain political cover. Both victim groups sit near the target end but by different mechanisms — marginalized populations are trapped by their immediate, non-transferable exposure to present algorithmic harm and have zero representation in bridging venues; future humanity is trapped by nonexistence and total lack of voice, represented only through proxies who must adopt bridging vocabulary to be heard at all. The two single-focus research communities occupy an intermediate position: organized and capable of some resistance, but structurally constrained because the bridging institutions increasingly gatekeep the funding and policy-access channels both need.
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge framework does not merely repackage extraction as coordination — it does contain a real coordination function (fragmented, competing advocacy was a genuine coordination failure with real costs), which is why this reads as tangled_rope rather than snare. But the founding problem (fractured discourse, duplicated infrastructure) has plausibly mutated into a narrower problem (concentrated gatekeeping by a small set of broker actors) rather than being resolved, per the founding_problem_status: contested. Classifying this as tangled_rope rather than collapsing it into either pure rope (which would erase the extraction from both victim populations) or pure snare (which would erase the genuine cross-field synthesis value) is exactly the discrimination this framework exists to make.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_entanglement_vs_institutional_convenience,
    'Are present AI harms and existential risks actually structurally entangled at the technical and institutional level (shared interventions, shared root causes), or is the entanglement claim primarily useful to the institutions that benefit from administering a unified frame?',
    'Technical audit of whether specific governance interventions (interpretability research, red-teaming infrastructure, model evaluation regimes, auditing standards) actually serve both risk classes simultaneously, versus case studies where ''unified'' interventions were technically inert for one risk class and existed only to satisfy funding requirements.',
    'If entanglement is substantially genuine, the tangled_rope classification understates the coordination function and the constraint sits closer to rope. If entanglement is substantially rhetorical, the constraint sits closer to a snare wearing coordination cover, with broker institutions as the primary extractive party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_entanglement_vs_institutional_convenience, empirical, 'Whether the bridge claim reflects real technical entanglement or institutional convenience for broker actors.').

omega_variable(
    committer_structure_bridge_reading_of_kernel,
    'This constraint is one reading (bridge_reading) of the ai_risk_governance_priority kernel, which also has an existential_risk_reading (strict priority for catastrophic scenarios) and a near_term_harms_reading (strict priority for present documented harm). Where is the disagreement between readings actually located?',
    'Each sibling reading is a separate constraint file with independent ε and stakeholder structure. The disagreement is located specifically in whether the two risk classes are best modeled as sharing a resource pool and governance apparatus (this reading) or as requiring strict triage under scarce attention and funding (both sibling readings, which disagree with each other about which triage direction is correct but agree that triage rather than integration is the right model).',
    'If the sibling readings'' claim (that entanglement forces dilution of both concerns) is empirically vindicated, this bridge reading''s coordination story weakens and it drifts toward the tangled_rope/snare boundary. If this reading''s claim (that shared interventions genuinely serve both) is vindicated, the sibling readings'' triage framing is undermined and resource competition between them is revealed as a false economy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_bridge_reading_of_kernel, conceptual, 'Documents this constraint''s position as bridge_reading within the ai_risk_governance_priority kernel contest, and the specific locus of disagreement with existential_risk_reading and near_term_harms_reading.').

omega_variable(
    broker_fragility_single_point_of_failure,
    'Given that the bridging function depends on a small number of high-centrality broker institutions rather than distributed cross-field collaboration, what happens to the coordination function if those specific brokers lose funding, credibility, or personnel?',
    'Track cross-field citation and funding-flow data over time for concentration trends; model counterfactual field fragmentation if the top-decile broker institutions were removed from the network.',
    'High fragility (coordination collapses without the specific brokers) supports classifying the arrangement as structurally extractive rent-collection dressed as necessary infrastructure. Low fragility (the field would re-coordinate around other actors) supports a more benign coordination reading where current brokers are incidental rather than essential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_fragility_single_point_of_failure, empirical, 'Whether the bridge framework''s coordination function is robust to broker turnover or dependent on specific gatekeeping actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_governance_priority__bridge_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_governance_priority__bridge_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_governance_priority__bridge_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.1).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_risk_governance_priority kernel. existential_risk_reading claims strict priority for catastrophic-scenario prevention (victim: future humanity only; beneficiary: longtermist research infrastructure). near_term_harms_reading claims strict priority for documented present harm (victim: marginalized populations only; beneficiary: near-term-harm advocacy and policy infrastructure). This bridge_reading claims non-exclusivity and structural entanglement (victim: both populations; beneficiary: cross-field broker institutions). All three share the same underlying kernel contest over how AI risk governance should allocate attention and resources, but each instantiates a structurally distinct constraint with its own ε, its own beneficiary/victim structure, and its own classification — per the ε-invariance principle, these are three files, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
