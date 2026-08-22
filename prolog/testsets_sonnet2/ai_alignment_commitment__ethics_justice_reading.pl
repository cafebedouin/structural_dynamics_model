% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Present-Harm and Bias Mitigation (Ethics/Justice Reading)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   Under the ethics/justice reading, 'AI alignment' names the work of
 *   detecting and mitigating bias in deployed systems: discriminatory lending
 *   and hiring models, exploitative content-moderation labor pipelines, and
 *   algorithmic scoring systems that harm gig workers and marginalized
 *   applicants today. This reading built real institutional infrastructure —
 *   audit firms, fairness benchmarks, trust-and-safety teams — that gives
 *   real present harms a name and a research community. But the same
 *   infrastructure has become the primary legible, fundable, PR-safe form of
 *   'doing alignment' inside companies and labs, which pulls funding and
 *   prestige away from long-horizon control research and produces a
 *   compliance-reporting apparatus that documents harm more reliably than it
 *   remediates it for the people actually affected.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.58).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.52).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Present-Harm and Bias Mitigation (Ethics/Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'f631e374-45c5-44f1-844b-71e44f259747').
narrative_ontology:cs_kernel_codification('f631e374-45c5-44f1-844b-71e44f259747', distributed).
narrative_ontology:cs_authority_grounding('f631e374-45c5-44f1-844b-71e44f259747', distributed).
narrative_ontology:cs_reading_relation('f631e374-45c5-44f1-844b-71e44f259747', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('f631e374-45c5-44f1-844b-71e44f259747', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('f631e374-45c5-44f1-844b-71e44f259747', foundational, present_demonstrated_harm_has_moral_priority_over_speculative_future_harm).
narrative_ontology:cs_axiom_status(present_demonstrated_harm_has_moral_priority_over_speculative_future_harm, holdable).
narrative_ontology:cs_axiom_grounding('f631e374-45c5-44f1-844b-71e44f259747', present_demonstrated_harm_has_moral_priority_over_speculative_future_harm, deontological).
narrative_ontology:cs_axiom('f631e374-45c5-44f1-844b-71e44f259747', foundational, marginalized_communities_bearing_current_algorithmic_harm_are_the_proper_referent_of_alignment_success).
narrative_ontology:cs_axiom_status(marginalized_communities_bearing_current_algorithmic_harm_are_the_proper_referent_of_alignment_success, holdable).
narrative_ontology:cs_axiom_grounding('f631e374-45c5-44f1-844b-71e44f259747', marginalized_communities_bearing_current_algorithmic_harm_are_the_proper_referent_of_alignment_success, conventional).
narrative_ontology:cs_reference_frame('f631e374-45c5-44f1-844b-71e44f259747', bias_audit_and_disparate_impact_framework).
narrative_ontology:cs_drift_state('f631e374-45c5-44f1-844b-71e44f259747', post_generative_ai_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f631e374-45c5-44f1-844b-71e44f259747', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, fairness_research_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, algorithmic_audit_consultancies).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, corporate_trust_and_safety_teams).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, gig_platform_workers_subject_to_algorithmic_scoring).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, marginalized_loan_and_hiring_applicants).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, content_moderators_in_low_wage_markets).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers_defunded_by_reallocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the operational definition of 'alignment' inside major labs and conferences as bias auditing, dataset debiasing, and fairness-metric compliance. Administers grant cycles, publication norms, and hiring pipelines around this definition. Can redirect institutional attention and funding toward this framing at will, and largely controls what counts as evidence of alignment progress in this domain.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, fairness_research_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell bias-audit and fairness-certification services to companies seeking to demonstrate alignment compliance. Revenue depends on the ethics/justice framing remaining the dominant operational meaning of alignment; if attention shifts fully to catastrophic-risk framing, this market contracts. Can pivot service offerings if the definition shifts, giving moderate exit.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, algorithmic_audit_consultancies, beneficiary,
    organized, biographical, mobile, national).

% Adopt bias-mitigation checklists and fairness dashboards as the visible, reportable form of 'doing alignment,' which satisfies regulators and press without requiring architectural changes to the underlying systems that generate the harms. Benefit from a framing that is auditable, PR-legible, and does not require slowing deployment timelines.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, corporate_trust_and_safety_teams, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, corporate_trust_and_safety_teams, agenda_setter).

% Are scored, ranked, and deactivated by algorithmic systems whose bias audits are typically conducted post-hoc and voluntarily by the platform, with no worker standing to compel remediation. Fairness reports rarely change deactivation outcomes for individuals; workers cannot exit the scoring system without losing income, and have no seat in defining what 'fair' means for their case.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, gig_platform_workers_subject_to_algorithmic_scoring, payer,
    powerless, immediate, trapped, national).

% Are denied credit or employment by models whose disparate impact is documented in academic fairness literature but rarely triggers binding correction; institutions cite ongoing 'fairness research' as evidence of good-faith effort while the scoring systems that harmed them remain in production. Cannot decline to be scored if they want the loan or job.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_loan_and_hiring_applicants, payer,
    powerless, biographical, trapped, national).

% Perform the labeling and adjudication work that fairness/bias datasets depend on, often under psychologically harmful conditions and low pay, in service of alignment work that primarily benefits the systems and companies that employ them, not their own working conditions. The 'present harm' framing rarely extends to their own labor conditions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, content_moderators_in_low_wage_markets, payer,
    powerless, immediate, trapped, global).

% Work on catastrophic-risk and control problems in labs where funding, headcount, and publication prestige increasingly flow toward the demonstrated-present-harm framing because it is easier to fund, publish, and defend to boards; they experience institutional pressure to reframe their work in bias/justice terms or lose resources, regardless of their own assessment of where the greatest risk lies.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers_defunded_by_reallocation, payer,
    moderate, civilizational, constrained, global).

% Community organizations and directly affected populations who could specify what remedy they actually need (structural changes to scoring systems, not audits) are rarely given standing in the definition of what counts as 'alignment success'; labs and companies define the metrics and consult these groups, if at all, after the framework is already set.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, civil_society_and_affected_communities, excluded,
    powerless, generational, trapped, national).

% Study how the term 'alignment' is operationalized differently across institutions and track whether present-harm framing displaces catastrophic-risk framing in funding and policy attention, without a stake in either outcome.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__ethics_justice_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__ethics_justice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates attention and resources toward documented, present-tense harms — biased scoring, discriminatory lending and hiring models, exploitative labeling labor — that would otherwise be treated as externalities outside any single actor's responsibility. It gives affected communities a vocabulary and a research/advocacy infrastructure to name harms that were previously undiagnosed or diffuse.
% TRANSFER_FUNCTION: Moves institutional attention, research funding, headcount, and reputational credit toward bias-auditing and fairness-compliance work and away from long-horizon control research; moves compliance costs from companies (who absorb audit costs, which are modest relative to revenue) toward the affected communities (who absorb the harms not caught or corrected by the audit regime) and toward long-term safety researchers (who absorb defunding and institutional marginalization).
% ABSENT_VOICES: Directly affected communities (gig workers, loan/hiring applicants, low-wage content moderators) rarely have a formal seat in defining what remediation their harm requires; their objection would likely be that fairness audits are process compliance without binding outcome guarantees. Long-term safety researchers would object that reallocation is driven by fundability and PR-legibility rather than a considered risk assessment, but this dispute happens mostly within institutions, not in public.
% DISAPPEARANCE_RATIONALE: If this reading of alignment vanished overnight, corporate trust-and-safety teams would lose their primary compliance vocabulary and audit consultancies would lose a market — those arrangements would visibly rearrange. But whether the underlying harms (discriminatory scoring, exploitative labeling labor) would be better or worse addressed under a different framing is genuinely disputed between the readings' proponents, which is why this sits at contested rather than a clean verdict.
% FOUNDING_PROBLEM: Documented, empirically measurable cases of AI systems reproducing and amplifying race, gender, and class bias in high-stakes decisions (lending, hiring, criminal justice risk scoring, content moderation) with real people harmed in the present, not hypothetically in the future.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic bias audits (outside the labs and consultancies that profit from audit work), investigative journalism, and litigation records from affected individuals corroborate that the underlying harms are real and ongoing. However, whether the CURRENT institutional response (audit-and-report cycles) is closing that harm or merely documenting it repeatedly without remediation is attested only by the institutions running the audits themselves — no fully independent body currently verifies remediation outcomes at scale.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.44) are both mid-range and rising over the measured interval: the audit-and-report cycle has become increasingly the visible face of 'alignment' inside corporate structures, while binding remediation for individually harmed workers and applicants remains rare. Suppression (0.52) reflects that affected communities have no standing to compel outcome-based correction — they can document harm but not force remedy — and that safety researchers face real institutional pressure to reframe or lose resources, which is a soft but real coercive structure. accessibility_collapse (0.4) and resistance (0.62) reflect that alternatives (binding remediation frameworks, worker co-determination of fairness metrics) are not fully closed off — active advocacy and litigation continue — but the audit-compliance framing meets real resistance precisely because affected parties experience it as insufficient.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting fairness-research seat, this looks like a genuine, hard-won coordination achievement: naming and measuring harms that were previously invisible to institutional risk frameworks. From the payer seats — especially the powerless, trapped ones — the same structure looks like a reporting apparatus that produces audits instead of remedies. The engine should compute these as different seat classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Fairness-audit institutions and corporate trust-and-safety teams sit near the beneficiary end: they collect funding, prestige, and compliance credit from this framing being dominant, with mobile-to-institutional exit. The four payer groups sit near the target end for structurally different reasons: gig workers, loan/hiring applicants, and low-wage moderators are trapped and powerless, bearing harm the audit regime documents but rarely remedies; long-term safety researchers are moderate-power but constrained, bearing an institutional reallocation cost that is not physical harm but a resource and credibility transfer away from their assessed priority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented present-tense bias harm) is genuinely live — this is not a dead mandate coasting on inertia. What is contested is whether the CURRENT institutional response (audit-and-report cycles, fairness dashboards) still serves that founding problem or has drifted into a self-perpetuating compliance-signaling function that primarily benefits the audit industry and corporate reputational needs. The founding_problem_status is authored as 'live' rather than 'dead' precisely because the underlying harms are ongoing and real — but the disappearance_verdict is 'contested' because whether THIS SPECIFIC apparatus is the right or sufficient response to that live problem is exactly what the reading contest is about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (ethics_justice) of the ai_alignment_commitment kernel. The sibling readings are safety_control_reading (alignment as preventing catastrophic loss of control) and integrated_reading (alignment as simultaneous non-exclusive attention to both). Where exactly is the disagreement located: is it about which harms are more probable, which harms are more severe, or which harms are more legible/fundable to institutions?',
    'Comparative tracing of institutional funding and headcount allocation decisions against internal risk-assessment memos, if disclosed, would show whether reallocation toward the present-harm framing tracks a genuine probability/severity reassessment or tracks fundability and PR-legibility independent of risk assessment.',
    'If reallocation tracks legibility/fundability rather than risk reassessment, the ethics_justice reading''s extraction from long-term safety research is better characterized as an opportunistic institutional capture rather than a considered prioritization; if it tracks genuine reassessment, the extraction is a defensible reallocation cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the ethics_justice/safety_control kernel disagreement is actually located structurally.').

omega_variable(
    audit_to_remedy_gap,
    'Does the fairness-audit and bias-mitigation apparatus this reading built actually produce binding remediation for harmed individuals (gig workers deactivated unfairly, applicants denied credit/employment), or does it primarily produce documentation and reputational credit without proportionate remedy?',
    'Track outcomes for individuals who file bias complaints against audited systems: rate of overturned decisions, compensation, or system change versus rate of ''noted for future review'' outcomes, across a sample of audited platforms over several years.',
    'A low remedy rate would support classifying this reading''s operational apparatus as substantially theatrical (high theater_ratio, tangled_rope leaning snare); a high remedy rate would support a genuine rope/coordination reading closer to the beneficiaries'' own framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_to_remedy_gap, empirical, 'Whether bias audits produce binding remedy or mostly documentation.').

omega_variable(
    reallocation_zero_sum_assumption,
    'Is the funding/attention relationship between present-harm work and long-term safety research genuinely zero-sum (so that gains for one reading structurally extract from the other), or is the apparent tradeoff an artifact of current institutional budget structures that could be resolved by simply increasing total investment in alignment work broadly?',
    'Cross-institutional comparison of labs/agencies that have expanded total alignment budgets versus those that have held budgets flat while reallocating internally — if expansion cases show both framings growing together, the zero-sum framing is an artifact of budget constraints, not a structural feature of the kernel.',
    'If not genuinely zero-sum, victimizing long-term safety researchers via this reading is a policy choice (fixable by increasing total investment) rather than an intrinsic extraction of this reading''s operation, which would lower the reading''s authored extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_zero_sum_assumption, empirical, 'Whether the present-harm/long-term-safety tradeoff is intrinsic to the reading or an artifact of budget scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the ai_alignment_commitment kernel. ethics_justice_reading (this file) authors ε against the standing bias-audit-and-compliance apparatus, with victims drawn from communities experiencing present algorithmic harm and from long-term safety researchers experiencing institutional reallocation. safety_control_reading authors a structurally distinct constraint centered on catastrophic-risk prevention, with a different beneficiary/victim set (frontier labs and control researchers as beneficiaries; the public exposed to uncontrolled deployment as victims). integrated_reading authors a third constraint attempting to hold both non-exclusively, with its own distinct extraction profile (the cost of maintaining dual-track institutional attention). The three are not the same constraint measured differently — each has a stable, non-averaged ε specific to its own reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
