% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Siloed Alignment Practice Assessed Under the Integrated Reading
 *   domain: technology governance/AI ethics/risk assessment
 *
 * SUMMARY:
 *   The standing arrangement under contest is the institutional practice of
 *   AI alignment as it is actually organized: separate funding buckets,
 *   separate conferences, separate career ladders, and separate vocabularies
 *   for control problems (loss of control, misuse, robustness) and justice
 *   problems (bias, disparate impact, deployment harm). This story
 *   instantiates the integrated reading of the kernel
 *   ai_alignment_commitment, which holds that alignment requires simultaneous
 *   attention to both problem classes as non-exclusive components of one
 *   obligation. Epsilon's referent is that siloed arrangement, assessed by
 *   the integrated reading's own lights — not the integrated arrangement the
 *   reading would put in place. On that referent the reading finds
 *   substantial extraction: duplicated infrastructure, systematically
 *   uncovered intersection harms, and the pricing of residual risk onto
 *   parties with no seat. The sibling readings (safety_control_reading,
 *   ethics_justice_reading) are separate constraints in separate files,
 *   linked through network.affects_constraints; their contest is recorded in
 *   omegas, not averaged into this story's numbers.
 *
 * KEY AGENTS:
 *   - - ai_funding_institutions: Agenda setter (institutional/arbitrage) — administers the separate portfolios whose category boundaries constitute the constraint; bears almost none of its costs
 *   - - frontier_lab_safety_divisions: Primary beneficiary (powerful/identity_locked) — collects headcount, prestige, and internal leverage from the control-only framing
 *   - - fairness_audit_industry: Secondary beneficiary (organized/identity_locked) — collects revenue and citation streams from fairness remaining a separately procured service
 *   - - marginalized_user_populations: Primary target (powerless/trapped) — absorbs present-day deployment harms while alignment attention flows elsewhere
 *   - - future_persons: Co-target (powerless/trapped, civilizational horizon) — inherits whatever control properties the under-resourced side fails to secure
 *   - - integrated_alignment_researchers: Target of the enforcement machinery (moderate/constrained) — pays career costs for crossing the boundary the constraint maintains
 *   - - community_advocacy_orgs: Excluded voice (organized/constrained) — documents harms, admitted only as consulted outsiders
 *   - - ai_governance_standards_bodies: Analytical observer (institutional/analytical) — sees the coverage gaps, controls no funding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Siloed Alignment Practice Assessed Under the Integrated Reading").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technology governance/AI ethics/risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'a154dc26-e1ae-45a6-8455-abbe73e75a6c').
narrative_ontology:cs_kernel_codification('a154dc26-e1ae-45a6-8455-abbe73e75a6c', distributed).
narrative_ontology:cs_authority_grounding('a154dc26-e1ae-45a6-8455-abbe73e75a6c', distributed).
narrative_ontology:cs_reading_relation('a154dc26-e1ae-45a6-8455-abbe73e75a6c', ai_alignment_commitment__safety_control_reading, forecloses).
narrative_ontology:cs_reading_relation('a154dc26-e1ae-45a6-8455-abbe73e75a6c', ai_alignment_commitment__ethics_justice_reading, forecloses).
narrative_ontology:cs_axiom('a154dc26-e1ae-45a6-8455-abbe73e75a6c', foundational, alignment_problems_non_exclusive).
narrative_ontology:cs_axiom_status(alignment_problems_non_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('a154dc26-e1ae-45a6-8455-abbe73e75a6c', alignment_problems_non_exclusive, deontological).
narrative_ontology:cs_axiom('a154dc26-e1ae-45a6-8455-abbe73e75a6c', secondary, silo_neglect_compounds_harms).
narrative_ontology:cs_axiom_status(silo_neglect_compounds_harms, holdable).
narrative_ontology:cs_axiom_grounding('a154dc26-e1ae-45a6-8455-abbe73e75a6c', silo_neglect_compounds_harms, empirically_contingent).
narrative_ontology:cs_reference_frame('a154dc26-e1ae-45a6-8455-abbe73e75a6c', unified_stewardship_mandate).
narrative_ontology:cs_drift_state('a154dc26-e1ae-45a6-8455-abbe73e75a6c', contemporary_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a154dc26-e1ae-45a6-8455-abbe73e75a6c', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, frontier_lab_safety_divisions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, fairness_audit_industry).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, ai_funding_institutions).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, marginalized_user_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_persons).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers philanthropic and public research portfolios bucketed into separate AI-safety and AI-ethics/fairness programs, each with its own call text, review panel, and success metrics. Cross-cutting proposals routinely fail the eligibility screen of both buckets. Moving to integrated funding would mean dismantling established program categories and renegotiating with both grantee communities; the institutions themselves bear little of the cost of fragmented attention.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_funding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, ai_funding_institutions, beneficiary).

% Staff the control, robustness, and misuse-prevention teams inside frontier AI labs. Headcount, prestige, and internal leverage rest on alignment being framed as a technical control problem owned by their discipline. Researchers who drift toward justice questions tend to be reassigned out of the safety track; abandoning the framing altogether would forfeit the professional identity and career capital built inside it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, frontier_lab_safety_divisions, beneficiary,
    powerful, biographical, identity_locked, global).

% Consultancies, audit shops, and academic groups that measure and certify deployed systems for bias and disparate impact. Revenue and citation streams depend on fairness remaining a separately procured service with its own standards and liability regime; absorbing control questions would dissolve the niche that defines the firms and the field.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, fairness_audit_industry, beneficiary,
    organized, biographical, identity_locked, global).

% Communities that absorb the present-day failures of deployed systems — discriminatory lending decisions, surveillance miscalibration, wrongful content-filter flags — while the majority of alignment funding chases longer-horizon control risks. They hold no seat in lab roadmapping or grant review; their recourse is after-the-fact complaint or litigation against systems they did not choose.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_user_populations, payer,
    powerless, biographical, trapped, global).

% People not yet alive who will inherit whatever control properties today's systems lock in. Every budget cycle that allocates alignment attention exclusively to present-day audits leaves their exposure unrepresented; they appear on no review panel, hold no veto, and cannot exit the arrangements made on their behalf.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_persons, payer,
    powerless, civilizational, trapped, universal).

% Researchers working across both problem classes — dual-use dynamics, bias inside safety classifiers, justice implications of containment policy. Their papers fit neither community's flagship venues cleanly, their grant applications fail both buckets' eligibility screens, and hiring committees in each specialty discount the half of the portfolio that belongs to the other.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers, excluded).

% Civil-society organizations documenting deployment harms and campaigning for affected communities. They request seats in safety evaluations and red-team exercises and are generally admitted only as consulted outsiders after design decisions are already made.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, community_advocacy_orgs, excluded,
    organized, generational, constrained, regional).

% Standards consortia and advisory councils drafting evaluation frameworks for advanced AI systems. They take input from both communities, watch the boundary's effect on coverage gaps, and can recommend integrated assessment requirements, but they control no research funding.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_governance_standards_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, frontier_lab_safety_divisions).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides scarce alignment expertise between two deep specializations — control/robustness research and bias/justice auditing — so that each develops tools, metrics, benchmarks, and review communities the other cannot easily replicate.
% TRANSFER_FUNCTION: Moves funding, talent, and publication attention away from generalist or integrated alignment work and into the two specialist pipelines; moves the residual costs — unmitigated bias harms today, unmitigated capability risks later — onto populations with no seat in either pipeline.
% ABSENT_VOICES: Marginalized user populations have no seat in frontier-lab roadmapping; future persons have no seat anywhere; community advocacy organizations sit outside both grant review and conference programming; integrated researchers are scattered along the margins of both communities rather than seated in either.
% DISAPPEARANCE_RATIONALE: If the siloed structure vanished overnight, funders would re-bucket portfolios within a budget cycle, merged review tracks and hybrid roles would proliferate, and the two specialist communities would lose the protected funding lines and distinct identities the boundary maintains; deployment decisions would face combined control-and-justice review instead of sequential or absent review.
% FOUNDING_PROBLEM: When both fields scaled in the early 2010s, expertise was genuinely thin: too few qualified researchers to staff rigorous control research and rigorous bias auditing simultaneously, so institutions specialized and built separate pipelines, each treating the other's problem class as either speculative or parochial.
% FOUNDING_PROBLEM_CORROBORATION: The specialist communities attest that scarcity persists and justifies triage. Corroborating sources outside the benefiting parties — national AI advisory reports recommending integrated assessment frameworks, science-of-science studies of funding fragmentation, and published incident audits cataloguing harms each pipeline failed to catch — attest that the scarcity is now partly manufactured by portfolio design, and affected-community documentation attests the justice half was never adequately resourced at all.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.65 because the silo misallocates a large share of total alignment effort: infrastructure is duplicated, intersection harms fall between the two pipelines' mandates, and each community systematically prices its uncovered risks onto the other's jurisdiction and onto unseated parties. Suppression is 0.60 and is a raw structural property, unscaled by power or scope — it reflects the enforcement machinery itself: eligibility screens that reject cross-cutting grants in both buckets, reviewer-pool composition, hiring signals, and reputational sanction against integrators. Theater is 0.41 and rising: joint statements, responsible-AI summits, and ethics boards without authority now consume a meaningful share of boundary-crossing energy while changing little. Accessibility collapse is 0.48 — integrated alternatives exist and are visible (hybrid roles, interdisciplinary venues) but carry measurable penalties, so alternatives persist rather than collapsing as they would under a natural law. Resistance is 0.58: integrators keep publishing, some funders experiment with pooled calls, and the boundary requires continuous maintenance rather than passive acceptance. The three measurement series share one grid (points 0, 3, 6, 9, 12, 15) so every metric is authored at every examined time point; all trajectories rise monotonically — extraction accumulates as deployed-system stakes grow, theatrical integration substitutes for functional integration, and enforcement hardens as integration pressure mounts. Identity-lock dynamics matter at the beneficiary seats: safety divisions are fused with the existential-steward identity and audit firms with the civil-rights-technologist identity; if either frame broke, the cost of integration would drop sharply for that seat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. From the funding institutions' position the arrangement is a legible, administrable division of labor they built and can defend; from the safety and audit seats it is a protected niche fused with professional identity; from the marginalized-population and future-persons seats it is pure unrepresented exposure; from the integrated-researcher seat it is a gatekeeping regime that taxes exactly the work the moment demands. The engine derives these per-seat classifications from the structural data; the divergence between seats is the finding, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (frontier_lab_safety_divisions, fairness_audit_industry, and the funding institutions in their secondary role) sit near the beneficiary end of directionality — the constraint subsidizes them, and their identity_locked exit does not push them toward the target end because they are not targets. The victims sit near the full-target end: marginalized_user_populations and future_persons are powerless and trapped, so their effective extraction is amplified; integrated_alignment_researchers are moderate-power and constrained, targeted by the enforcement layer specifically. The funding institutions' arbitrage-grade exit damps their personal exposure further. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope (specialization is just efficient division of labor) would erase the asymmetric extraction — the unseated parties who carry the residual risk and the integrators taxed by the boundary. Reading it as pure snare (the coordination story is mere cover) would erase the genuine coordination value: deep control research and deep audit methodology are real goods that shallow generalism would not produce. The founding problem — expertise too thin to staff both fronts — is contested rather than dead: specialists attest it lives, external corroboration (advisory-body reports, funding-fragmentation studies, incident audits) attests it is now partly manufactured by the portfolio design itself. Because the status is contested rather than dead, the mismatch consumer does not fire a zombie flag, but the rising theater_ratio series marks the arrangement as accumulating the raw material of a later piton phase if integration continues to be performed rather than practiced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel ai_alignment_commitment. Would instantiating safety_control_reading or ethics_justice_reading instead produce a different victim set and a different epsilon for the same underlying commitment?',
    'Comparative classification across the three sibling stories: the safety reading narrows the victim set to future persons exposed to loss-of-control; the justice reading narrows it to present marginalized populations; the integrated reading carries both. Divergent computed types across the family locate the disagreement.',
    'Under a sibling reading, epsilon shifts materially (each sibling prices the other''s neglected harm class as outside the constraint), and the tangled_rope verdict here would not transfer — the family exists precisely because the readings are not one constraint measured three ways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the alignment kernel is instantiated, and what the siblings would change.').

omega_variable(
    scarcity_real_or_manufactured,
    'Is the expert scarcity that ostensibly forces triage between control and justice work genuine, or is it produced by the portfolio architecture that separates the two funding streams?',
    'Longitudinal funding-flow analysis tracking whether integrated proposals are rejected for quality or for category ineligibility, plus counterfactual evidence from jurisdictions or funders that ran integrated programs and retained specialist depth.',
    'If scarcity is manufactured, the coordination defense of the silo weakens, effective extraction rises, and the arrangement slides toward snare; if genuine, part of the measured extraction is the unavoidable price of depth under constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_real_or_manufactured, empirical, 'Whether the founding scarcity is a fact of the world or an artifact of the arrangement.').

omega_variable(
    intersection_harm_attribution,
    'What fraction of realized harms — biased deployments, unmitigated capability risks, interaction failures such as biased safety classifiers — is attributable to silo neglect rather than to irreducible technical uncertainty?',
    'Structured incident audits coding each harm by which community''s existing tools would plausibly have caught it, reviewed by assessors outside both communities.',
    'Higher attribution raises epsilon and strengthens the case for mandated integration; lower attribution supports the specialization-efficiency defense and lowers the extractive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersection_harm_attribution, empirical, 'How much of the harm record the silo itself caused, as opposed to the underlying difficulty of the problems.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is specialist resistance to integration primarily structural (eligibility screens, review gatekeeping, hiring signals) or internalized (professional identities that make cross-work feel like defection)?',
    'Post-exit trajectory study: track researchers who leave specialist communities and observe whether anti-integration stances persist once the gatekeeping incentives are removed.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure — the boundary travels inside the agents and survives formal reform of funding categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind the enforced boundary.').

omega_variable(
    kernel_codification_framing,
    'Is the alignment kernel best modeled as distributed (an ambiguous commitment with no adjudicating authority, as authored here) or as quasi-formalized around a small canon of landmark alignment proposals that function as de facto authoritative texts?',
    'Citation-genealogy analysis: if a handful of papers operate as load-bearing references that practitioners treat as settling what alignment means, the quasi-formalized framing fits; if meaning is negotiated community-by-community, distributed fits.',
    'Under the quasi-formalized framing, the commitment system acquires designated interpreters and the drift analysis reroutes through interpretive-layer dynamics rather than diffuse negotiation, changing the computed CS pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Alternative framings of how the alignment kernel is codified and adjudicated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aic_ir_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(aic_ir_tr_t3, ai_alignment_commitment__integrated_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(aic_ir_tr_t6, ai_alignment_commitment__integrated_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(aic_ir_tr_t9, ai_alignment_commitment__integrated_reading, theater_ratio, 9, 0.32).
narrative_ontology:measurement(aic_ir_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(aic_ir_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.41).

% Extraction over time
narrative_ontology:measurement(aic_ir_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aic_ir_be_t3, ai_alignment_commitment__integrated_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(aic_ir_be_t6, ai_alignment_commitment__integrated_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(aic_ir_be_t9, ai_alignment_commitment__integrated_reading, base_extractiveness, 9, 0.57).
narrative_ontology:measurement(aic_ir_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(aic_ir_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(aic_ir_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(aic_ir_su_t3, ai_alignment_commitment__integrated_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(aic_ir_su_t6, ai_alignment_commitment__integrated_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(aic_ir_su_t9, ai_alignment_commitment__integrated_reading, suppression_requirement, 9, 0.51).
narrative_ontology:measurement(aic_ir_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(aic_ir_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ethics_justice_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI alignment' decomposes into three structurally distinct constraints corresponding to the three declared readings of the kernel ai_alignment_commitment. The safety_control_reading instantiates a constraint whose victim set is future persons exposed to loss-of-control; the ethics_justice_reading instantiates one whose victim set is present marginalized populations; this integrated_reading instantiates one whose victim set is both, priced by the siloed arrangement that separates the other two. The epsilon values differ across the family because each reading assesses the standing arrangement by its own lights — the readings are not one constraint measured three ways. The two specialist readings are upstream in the sense that their institutional products (control benchmarks, fairness audit standards) are cited as evidence that the separated pipelines suffice; this reading exerts structural pressure on both by contesting their exclusivity premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
