% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated Alignment Priority: Catastrophic and Present Harms as Complementary
 *   domain: AI governance/technology ethics/risk assessment
 *
 * SUMMARY:
 *   This constraint instantiates the 'integrated' reading of the contested AI
 *   alignment priority kernel: the claim that catastrophic-risk work
 *   (preventing loss of control over advanced systems) and present-harm work
 *   (preventing discriminatory/extractive deployment harms) are complementary
 *   rather than competing, and that alignment practice should pursue both
 *   through dual methodology (red-teaming plus deployment audits) with
 *   balanced resource allocation. This is a distinct constraint from the
 *   existential_risk_reading (which treats catastrophic prevention as the
 *   priority) and the nearterm_harms_reading (which treats present-day
 *   justice as the priority) — each reading has its own beneficiary/victim
 *   structure and its own epsilon, and this file authors only the integrated
 *   reading's structure, per the epsilon-invariance principle. The integrated
 *   reading's own coordination story is genuine (it does reduce destructive
 *   infighting between the two research communities and expand the combined
 *   resource pool) but the same structure that coordinates also lets
 *   institutions defer concrete remediation to either wing while claiming
 *   credit for both, which is the extraction this story documents.
 *
 * KEY AGENTS:
 *   - frontier_lab_governance_teams: sets and enforces the resource-allocation policy that operationalizes 'balance' between the two priorities
 *   - ai_safety_research_institutions and compliance_consultancies: institutional beneficiaries who gain funding and legitimacy from the dual-track framing regardless of whether either track is adequately resourced
 *   - marginalized_deployment_affected_communities: bear immediate, documented harm when present-harm remediation loses the internal allocation fight
 *   - future_populations: bear catastrophic-risk if capability-control work is diluted by mandated parity with present-harm work
 *   - existential_risk_advocates and nearterm_justice_advocates: the two excluded factions of the underlying kernel contest, each of whom believes the integrated framing structurally disadvantages their priority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.48).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.42).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated Alignment Priority: Catastrophic and Present Harms as Complementary").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "AI governance/technology ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '02ac9ac3-3a57-47e6-ae3e-6976727f6734').
narrative_ontology:cs_kernel_codification('02ac9ac3-3a57-47e6-ae3e-6976727f6734', distributed).
narrative_ontology:cs_authority_grounding('02ac9ac3-3a57-47e6-ae3e-6976727f6734', distributed).
narrative_ontology:cs_reading_relation('02ac9ac3-3a57-47e6-ae3e-6976727f6734', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('02ac9ac3-3a57-47e6-ae3e-6976727f6734', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('02ac9ac3-3a57-47e6-ae3e-6976727f6734', foundational, harm_priorities_are_jointly_addressable_without_tradeoff).
narrative_ontology:cs_axiom_status(harm_priorities_are_jointly_addressable_without_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('02ac9ac3-3a57-47e6-ae3e-6976727f6734', harm_priorities_are_jointly_addressable_without_tradeoff, empirically_contingent).
narrative_ontology:cs_axiom('02ac9ac3-3a57-47e6-ae3e-6976727f6734', secondary, institutional_resource_allocation_can_be_genuinely_balanced_rather_than_sequenced).
narrative_ontology:cs_axiom_status(institutional_resource_allocation_can_be_genuinely_balanced_rather_than_sequenced, holdable).
narrative_ontology:cs_axiom_grounding('02ac9ac3-3a57-47e6-ae3e-6976727f6734', institutional_resource_allocation_can_be_genuinely_balanced_rather_than_sequenced, instrumental).
narrative_ontology:cs_reference_frame('02ac9ac3-3a57-47e6-ae3e-6976727f6734', pre_fracture_unified_alignment_field).
narrative_ontology:cs_drift_state('02ac9ac3-3a57-47e6-ae3e-6976727f6734', contemporary_resource_competition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02ac9ac3-3a57-47e6-ae3e-6976727f6734', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_safety_research_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, compliance_consultancies).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, frontier_lab_governance_teams).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, under_resourced_safety_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, under_resourced_safety_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the internal policy that allocates researcher time, compute, and audit budget across both catastrophic-risk red-teaming and present-harm bias/fairness audits. Decides the split, sets the vocabulary of 'complementary priorities,' and reports the arrangement to regulators and the public as evidence of comprehensive alignment practice.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, frontier_lab_governance_teams, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives funding, prestige, and staffing under the integrated framing, which lets both existential-risk researchers and fairness/justice researchers claim a seat at the same table and draw from the same grant pools. Benefits from the framing's legitimacy even where the two research programs compete internally for the same finite resources.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_safety_research_institutions, beneficiary,
    organized, generational, mobile, global).

% Sells combined 'catastrophic + present harms' audit packages to labs seeking to demonstrate balanced diligence. Profits from the complexity of running two distinct methodologies (red-teaming and deployment audits) under one contract, regardless of whether either is done with adequate depth.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, compliance_consultancies, beneficiary,
    organized, biographical, mobile, global).

% Experiences discriminatory lending, hiring, and moderation decisions from deployed systems today. Their harms are documented and immediate, but resourcing decisions inside labs routinely reallocate audit budget toward speculative catastrophic scenarios when the two priorities compete for the same quarter's compute and headcount, leaving present-harm remediation underfunded relative to its urgency.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_deployment_affected_communities, payer,
    powerless, immediate, trapped, national).

% Cannot advocate for their own interests in current resource allocation. Bear the cost if catastrophic-risk work is diluted by the integrated framing's insistence on parity with present-harm work, even when a given capability jump genuinely warrants concentrated existential-safety attention rather than a fixed split.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, global).

% Individual researchers and auditors are told to pursue both red-teaming and deployment-harm audits with headcount sized for one. They benefit from the integrated framing's legitimacy (their work is validated as part of 'real alignment') but pay in burnout, methodological compromise, and the political cost of choosing which harm gets attention this cycle.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, under_resourced_safety_teams, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, under_resourced_safety_teams, beneficiary).

% Argue the integrated framing dilutes urgent capability-control work by mandating parity with present-harm concerns that, in their view, do not carry comparable stakes. Present within the broader discourse but structurally out-voted whenever resource allocation defaults to the 'balanced' split this reading requires.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% Argue the integrated framing lets labs point to catastrophic-risk work as cover while deployment harms against marginalized groups continue unaddressed at scale. Present in the discourse but structurally out-voted whenever labs invoke 'both priorities matter' to defer concrete present-harm remediation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_justice_advocates, excluded,
    organized, immediate, constrained, global).

% Evaluates whether labs' claimed dual-methodology compliance (red-teaming plus audits) reflects genuine balanced practice or theatrical box-checking. Can compel disclosure of actual resource splits and audit depth.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, regulators_and_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and resource-allocation frame that lets safety researchers working on divergent timescales and harm-types (speculative capability loss-of-control vs. documented present-day discrimination) coordinate inside the same institutions, budgets, and public accountability processes instead of running wholly separate, competing movements.
% TRANSFER_FUNCTION: Moves institutional legitimacy, funding, and compliance credit toward labs and consultancies that can demonstrate a dual-track program, while moving genuine remediation attention away from whichever harm-type loses the internal allocation fight in a given cycle — most often present, documented harms lose ground to speculative future ones when frames of urgency compete for the same finite audit budget.
% ABSENT_VOICES: Both wings of the underlying kernel contest — existential-risk advocates who think the framing dilutes urgent capability-control work, and near-term-justice advocates who think it launders present harms behind catastrophic-risk theater — are present in the discourse but structurally overridden by the integrated framing's built-in claim that the tension between them is already resolved.
% DISAPPEARANCE_RATIONALE: If the integrated framing vanished, labs would revert to whichever single-priority framing their internal politics favor. Existential-risk-dominant labs would concentrate resources on capability control and present-harm remediation would likely shrink further; near-term-justice-dominant labs would do the reverse. Whether this counts as the world rearranging or reverting to a prior equilibrium is itself contested between the two excluded factions — each believes the integrated framing is currently suppressing their preferred allocation, so its removal would 'rearrange' the world in their favor, not the other's.
% FOUNDING_PROBLEM: Two legitimate alignment research communities — one focused on loss-of-control risk from advanced systems, one focused on documented discriminatory and extractive harms from deployed systems — were competing for the same funding, talent, and institutional attention, threatening to fracture the alignment field into rival camps that undermine each other's political standing.
% FOUNDING_PROBLEM_CORROBORATION: Institutional funders and philanthropic foundations attest the integrated framing genuinely reduced destructive infighting and expanded the total resource pool for both research programs. Independent policy researchers outside both funded communities (e.g., academic science-and-technology-studies scholars tracking AI governance discourse) attest that in practice the framing is frequently invoked by labs to defer concrete near-term remediation commitments while claiming credit for balance — corroboration is genuinely split rather than unanimous, and no source entirely outside institutions with a funding stake in one wing or the other has fully validated the 'resolved' reading.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the integrated framing does perform genuine coordination work (reduced destructive infighting, expanded combined resourcing) alongside its extraction (institutional credit-claiming that outpaces actual remediation depth on either track). This is lower than either single-priority reading would likely show in isolation, because the integrated reading's explicit mandate to serve both reduces (without eliminating) the risk of either being fully sacrificed for the other. Suppression is moderate (0.42): the framing does not forcibly prevent either advocacy faction from speaking, but it structurally out-votes both by defining the 'reasonable' middle position, which functions as a soft suppression of both extremes. Theater ratio rises over the interval (0.20 to 0.38) reflecting an accumulating pattern where labs increasingly perform the dual-methodology commitment (running audits, publishing red-team reports) at a pace that outstrips the depth or consequence of either. All three metrics share one time grid across the 24-unit interval.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (frontier lab governance teams), the arrangement looks like principled, comprehensive risk management — a mature institution taking both catastrophic and present harms seriously. From either payer seat (marginalized communities bearing present harm, or future populations bearing tail catastrophic risk), the same arrangement looks like a resource-allocation mechanism that structurally guarantees neither harm gets full attention, because both compete for the same finite budget under a framing that forecloses prioritizing either.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (research institutions, consultancies, governance teams) collect funding, legitimacy, or contract revenue from operating or servicing the dual-track program — low directionality, near the subsidized end. Victims split into two structurally distinct groups: marginalized_deployment_affected_communities bear immediate, present, documented harm (trapped exit, powerless, immediate horizon) and future_populations bear speculative, deferred, potentially catastrophic harm (trapped exit by definition, powerless, civilizational horizon) — both pushed toward the target end of directionality, but via different mechanisms (resource dilution vs. resource dilution in the opposite direction, depending on which cycle's allocation fight they lose). under_resourced_safety_teams sit dual-positioned: they benefit from institutional validation of their work as 'real alignment' while paying the operational cost of being tasked with two jobs' worth of work on one job's budget.
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated reading's founding problem — destructive competition between two legitimate research communities for the same institutional attention — was genuinely live at founding and arguably remains partially live (per founding_problem_status: contested). This prevents a simple 'pure extraction' misclassification: the coordination function is real and independently corroborated by funders who observed reduced infighting. But the framing has also become a mechanism institutions use to defer concrete commitment to either priority, which the tangled_rope classification captures precisely by requiring both a genuine beneficiary/coordination structure AND a genuine victim/extraction structure under active enforcement (the allocation policy itself). Neither existential_risk_reading nor nearterm_harms_reading alone would show this dual structure — that is the structural delta this reading is authored to isolate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_complementarity_vs_institutional_convenience,
    'Is the claim that catastrophic and present harms are genuinely complementary (addressing one strengthens capacity to address the other) a structurally sound methodological claim, or is it primarily a framing that lets institutions avoid the harder political choice of prioritizing one over the other?',
    'Track whether labs operating under the integrated framing show measurable improvement on BOTH present-harm remediation metrics (documented discrimination incidents, audit remediation rates) AND catastrophic-risk metrics (red-team finding severity, capability control incident rates) over time, relative to labs operating under either single-priority framing. Convergent improvement on both would support genuine complementarity; stagnation or decline on one while resources flow to institutional credit-claiming would support the convenience-framing hypothesis.',
    'If genuine complementarity is empirically supported, this reading''s coordination function dominates and the constraint moves toward a rope classification. If the convenience-framing hypothesis holds, the tangled_rope classification is confirmed and the extraction component is understated by current metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_complementarity_vs_institutional_convenience, empirical, 'Whether integrated framing produces real dual improvement or masks deferred prioritization.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which institutional and political forces determine whether a given lab, funder, or regulator adopts the integrated_reading versus one of the sibling readings (existential_risk_reading, nearterm_harms_reading) of the underlying alignment-priority kernel, and is that selection itself capturable by whichever faction currently holds more institutional power?',
    'Comparative institutional analysis: track funding sources, leadership composition, and public communications across labs that adopt each reading, and correlate with which reading each institution''s dominant internal faction favors.',
    'If reading-selection tracks institutional power rather than independent assessment of relative risk magnitude, the ''integrated'' framing may itself be a compromise position adopted primarily where neither faction can dominate outright — which would mean this reading''s prevalence is itself evidence of contested internal power rather than genuine synthesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether kernel-reading adoption tracks institutional power balance rather than independent risk assessment.').

omega_variable(
    resource_allocation_measurement_gap,
    'Is there a reliable, auditable methodology for verifying that a lab''s claimed ''balanced'' resource split between catastrophic and present-harm work reflects actual compute, headcount, and remediation-follow-through allocation, or only public communications framing?',
    'Regulatory or third-party audit access to internal resource allocation records (headcount-hours, compute-hours, remediation ticket closure rates) disaggregated by harm-type category, compared against public claims of balance.',
    'Absent verified allocation data, the theater_ratio measurements in this story rely on inference from public disclosure patterns rather than direct measurement; verified access would sharpen or revise the theater trajectory substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_measurement_gap, empirical, 'Whether claimed resource balance is independently verifiable or self-reported only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__integrated_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__integrated_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__integrated_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'AI alignment priority' per the epsilon-invariance principle: existential_risk_reading (Mountain-leaning claim treating catastrophic prevention as structurally necessary and non-negotiable), integrated_reading (this file; Tangled Rope, moderate epsilon on both harm types, dual victim set), and nearterm_harms_reading (likely Snare-leaning claim if present-harm remediation is shown as structurally deprioritized under status-quo framings). Each carries its own epsilon and its own stakeholder/victim structure; none is a measurement-basis variant of another. Network edges here record that this reading's institutional adoption structurally influences resource availability and legitimacy conditions for both sibling readings — a lab that adopts the integrated framing changes the political viability of either pure-priority framing at that institution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
