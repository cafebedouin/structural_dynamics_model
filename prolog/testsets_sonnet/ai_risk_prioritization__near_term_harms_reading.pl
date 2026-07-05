% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of AI Risk Prioritization
 *   domain: technology/social justice/governance
 *
 * SUMMARY:
 *   The near-term harms reading holds that AI risk is primarily constituted
 *   by measurable, present-tense discrimination, displacement, and
 *   surveillance produced by already-deployed systems, and that
 *   justice-oriented interventions (bias audits, worker protections,
 *   surveillance regulation) are the paramount response. This reading
 *   coordinates a genuine and empirically documented problem — algorithmic
 *   systems that reproduce and amplify racial, economic, and labor inequities
 *   right now — but it also structurally competes with the existential-risk
 *   reading for finite funding, legislative attention, and institutional
 *   legitimacy. The coordination function (protecting real present victims)
 *   is bundled with an extraction dynamic: research institutions,
 *   consultancies, and advocacy organizations that thrive under this framing
 *   benefit from characterizing the rival framing as a distraction, and this
 *   framing effect itself imposes an opportunity cost that its own metrics do
 *   not register.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.42).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.38).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology/social justice/governance").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '84bcf19f-ba98-4404-9a7a-eb007cf8c59b').
narrative_ontology:cs_kernel_codification('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', distributed).
narrative_ontology:cs_authority_grounding('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', distributed).
narrative_ontology:cs_reading_relation('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', foundational, present_measurable_harm_has_moral_priority).
narrative_ontology:cs_axiom_status(present_measurable_harm_has_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', present_measurable_harm_has_moral_priority, deontological).
narrative_ontology:cs_axiom('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', secondary, speculative_future_harm_claims_require_discounting).
narrative_ontology:cs_axiom_status(speculative_future_harm_claims_require_discounting, holdable).
narrative_ontology:cs_axiom_grounding('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', speculative_future_harm_claims_require_discounting, empirically_contingent).
narrative_ontology:cs_reference_frame('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', civil_rights_and_labor_justice_framework).
narrative_ontology:cs_drift_state('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', generative_ai_mainstreaming_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('84bcf19f-ba98-4404-9a7a-eb007cf8c59b', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, affected_marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_ethics_consultancies).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, gig_workers_subject_to_algorithmic_management).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_low_income_tenants).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, racialized_loan_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_deploying_corporations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, affected_marginalized_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_deploying_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas, audit standards, and policy language around algorithmic bias, disparate impact, and surveillance harms. Builds careers, institutes, and consultancies on documenting present-tense AI harms. Can pivot fields or institutions if the funding and policy attention shifts, but currently commands significant grant and legislative attention under this framing.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary).

% Racialized, low-wage, and surveilled populations who are subject to discriminatory scoring, algorithmic hiring/firing, predictive policing, and tenant surveillance today. Benefit when bias audits and worker protections are enacted in their favor, but have no direct control over whether resources are allocated toward auditing deployed systems versus toward speculative future-risk research. Cannot exit the systems that score and monitor them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, affected_marginalized_communities, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, affected_marginalized_communities, payer).

% Managed by opaque dispatch and rating algorithms that determine pay, deactivation, and scheduling. Bear the material cost of unaudited or under-regulated deployed systems in real time. Leaving the platform means losing income; there is no comparable alternative labor market in many regions.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, gig_workers_subject_to_algorithmic_management, payer,
    powerless, immediate, constrained, national).

% Live under facial recognition, package/behavior tracking, and automated eviction-risk scoring deployed by landlords and municipal systems. Cannot afford to relocate away from monitored housing stock; their exposure to present algorithmic harm is exactly the harm the near-term framing exists to document and remedy.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveilled_low_income_tenants, payer,
    powerless, immediate, trapped, local).

% Subject to credit and lending algorithms shown to reproduce historical racial bias. Bear the cost of discriminatory deployed systems now; benefit if bias-audit mandates funded under this reading actually change lending practice, but bear the risk if attention and funding drift toward speculative future scenarios instead.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, racialized_loan_applicants, payer,
    powerless, biographical, constrained, national).

% Sell bias-audit, fairness-certification, and compliance services to corporations deploying AI systems. Their market exists because the near-term harms framing establishes audits and disparate-impact review as the legitimate remedy; they can rebrand toward other regulatory frameworks if this one loses institutional favor.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_ethics_consultancies, beneficiary,
    moderate, biographical, mobile, national).

% Argue that AGI misalignment poses catastrophic and irreversible risk and that resources devoted to near-term bias/discrimination framing come at the expense of alignment research. Are frequently characterized within the near-term reading as speculative, unfalsifiable, or distraction from measurable present harm — they are present in the broader policy conversation but structurally marginalized within this reading's framing of what counts as legitimate AI risk.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_research_institutions, excluded,
    organized, civilizational, mobile, global).

% Face compliance costs (audits, disclosure, worker-protection rules) under the near-term harms framing, but also benefit from a regulatory environment that is comparatively tractable and reputation-manageable compared to being asked to internalize speculative extinction-level liabilities. Can lobby to shape which harms get audited and how strictly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_deploying_corporations, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, ai_deploying_corporations, beneficiary).

% Legislatures, foundations, and regulatory agencies deciding how to allocate attention and funding between near-term harms mitigation and long-horizon alignment research. Take testimony from both readings' proponents and can shift the balance of resources and legitimacy toward one framing or the other.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_and_funding_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates advocacy, research funding, and regulatory attention around documenting and remedying measurable present-day algorithmic harms to specific populations — bias audits, worker protections, surveillance limits — rather than diffusing effort toward speculative future scenarios.
% TRANSFER_FUNCTION: Moves regulatory attention, research funding, and compliance obligations toward auditing and remedying deployed-system harms; corporations bear compliance costs which are meant to transfer protection to affected populations, while attention and funding are correspondingly diverted away from long-horizon alignment research.
% ABSENT_VOICES: Existential-risk researchers and alignment-focused institutions are structurally present in the wider debate but are characterized within this reading as speculative or distracting, which limits their standing to compete for the same funding and legislative attention pools this reading claims for near-term harms.
% DISAPPEARANCE_RATIONALE: If the near-term harms framing disappeared as an organizing claim, bias-audit mandates, algorithmic worker-protection rules, and surveillance-regulation efforts currently justified under it would lose their primary legitimating rationale; funding and legislative attention would likely redirect toward alignment/x-risk framing or toward no AI-specific framing at all, and marginalized populations currently benefiting from audit-driven remedies would lose a key advocacy lever.
% FOUNDING_PROBLEM: Deployed AI and algorithmic systems were producing documented, measurable discriminatory and exploitative outcomes (biased hiring, credit, policing, and surveillance systems) affecting specific populations now, while public and funding attention risked being captured entirely by speculative long-horizon existential-risk narratives that offered no remedy for present harm.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical audits (e.g. academic algorithmic-bias studies, investigative journalism on lending and policing algorithms, labor organizing reports from gig workers) corroborate that measurable present-tense discriminatory and surveillance harms exist, from sources outside the fairness-accountability research and consultancy communities that benefit from this framing's funding and legitimacy.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).
:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by interval end) because the framing genuinely redirects some resources toward real remedies for real victims, but a portion of institutional funding and attention captured under this framing does not clearly flow back to the populations it names as its justification — instead sustaining the audit/consultancy/research ecosystem itself. Suppression is present but moderate (0.38): the mechanism is largely reputational and rhetorical (labeling rival concerns 'speculative distraction') rather than coercive exclusion, which is why suppression trails extractiveness rather than exceeding it. Theater ratio (0.30) reflects that a meaningful share of activity under this framing is genuine audit and remedy work, with a growing but still secondary share being compliance theater (corporations funding audits primarily for reputational cover rather than remedial change).
 *
 * PERSPECTIVAL GAP:
 *   From the vantage of affected communities and labor organizers, this reading is coordination that is working, however incompletely, against present harm. From the vantage of alignment/x-risk researchers, the same framing functions as an attention-capture mechanism that starves a different (and in their view more severe) risk category of resources by successfully rhetorically demoting it to 'speculative distraction.' The engine should register this as seat divergence rather than requiring either side's self-description to be authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   Affected marginalized communities and specifically named victim groups (gig workers, surveilled tenants, racialized loan applicants) sit near the target end of directionality: they are trapped or constrained in their exposure to deployed-system harms and depend on this framing's success for remedy, making them both notional beneficiaries of the framing's political success and structural payers of the underlying harms it responds to — hence their dual role. Fairness/accountability researchers and consultancies sit near the beneficiary end: they gain career capital, market position, and institutional standing from the framing's ascendancy regardless of whether remedies fully materialize. Existential-risk institutions are excluded rather than coordinated or extracted from — their marginalization is a structural byproduct of the framing's success, not itself a payment flow within this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (measurable, present-tense algorithmic discrimination and surveillance harm) remains live and empirically corroborated by sources outside the benefiting research/consultancy ecosystem, which is why founding_problem_status is 'live' rather than 'dead' — this distinguishes the constraint from mandatrophy. The risk is not that the founding problem has disappeared but that resource-allocation and attention-capture dynamics may persist and expand independent of whether they remain the most efficient remedy for the named victims, which is exactly the coordination/extraction hybrid a tangled_rope classification is meant to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_allocation_zero_sum,
    'Is attention/funding for near-term harms and existential risk genuinely zero-sum, such that this reading''s success structurally suppresses the rival reading''s resources, or can both be funded without meaningful tradeoff?',
    'Longitudinal tracking of total AI-safety-adjacent funding pools and legislative attention allocations across both framings over a multi-year window; if total pools grow to accommodate both without displacement, the zero-sum premise is falsified.',
    'If genuinely zero-sum, the near-term harms reading''s suppression of the existential-risk reading is a structural feature of its success, not incidental rhetoric — raising its effective extractiveness toward the rival framing''s resource base. If not zero-sum, the suppression is closer to reputational friction with lower structural cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_allocation_zero_sum, empirical, 'Whether near-term and existential-risk framings compete for a fixed or an expandable resource pool.').

omega_variable(
    remedy_capture_by_intermediaries,
    'What share of resources mobilized under the near-term harms framing actually reaches affected marginalized populations as material remedy, versus being captured by the audit/consultancy/research intermediary layer?',
    'Independent tracing of grant and compliance-spending flows from initial allocation to end outcomes (wage recovery, discrimination remedies, surveillance rollback) versus intermediary institutional budgets.',
    'High capture by intermediaries would support a higher extractiveness score and strengthen the tangled_rope classification (coordination cover for institutional rent-seeking); low capture would support a closer-to-rope reading with the intermediary layer as genuine infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_capture_by_intermediaries, empirical, 'Whether mobilized resources reach named victims or are absorbed by the intermediary ecosystem.').

omega_variable(
    reading_framing_underdetermination,
    'Is the choice to treat ''near-term harms'' and ''existential risk'' as two readings of one kernel (ai_risk_prioritization) itself the right decomposition, or are these better modeled as entirely independent risk categories with no shared kernel at all?',
    'Examine whether policy and funding bodies actually treat the two as substitutable claims on a single attention/resource budget (supporting the shared-kernel framing) or as orthogonal concerns funded from separate pools (supporting independence).',
    'If policy bodies treat them as substitutable, the shared-kernel/reading structure is correct and the competitive dynamic modeled here is real. If funded independently with no substitution effect, the ''reading of a kernel'' framing may overstate the structural connection between the two constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the two AI-risk framings genuinely share a contested kernel or are better modeled as independent constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(ai_r_tr_t36, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 36, 0.27).
narrative_ontology:measurement(ai_r_tr_t48, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 48, 0.29).
narrative_ontology:measurement(ai_r_tr_t60, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(ai_r_be_t36, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 36, 0.39).
narrative_ontology:measurement(ai_r_be_t48, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 48, 0.41).
narrative_ontology:measurement(ai_r_be_t60, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(ai_r_su_t36, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 36, 0.34).
narrative_ontology:measurement(ai_r_su_t48, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 48, 0.36).
narrative_ontology:measurement(ai_r_su_t60, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, existential_risk_reading).

% DUAL FORMULATION NOTE:
% This story and existential_risk_reading are siblings within the ai_risk_prioritization kernel family. near_term_harms_reading names present, empirically documented victim populations (gig workers, surveilled tenants, racialized loan applicants) and a 0-5 year timescale; existential_risk_reading names hypothetical future populations under AGI misalignment scenarios and a civilizational timescale. Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here because they structurally compete for the same funding and legislative attention pools, not because they are the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
