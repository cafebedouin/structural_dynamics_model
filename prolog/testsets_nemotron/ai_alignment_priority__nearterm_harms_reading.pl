% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment Priority: Near-Term Harm Prevention for Marginalized Populations
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the nearterm_harms_reading of the
 *   ai_alignment_priority kernel. It asserts that alignment's primary meaning
 *   is preventing discriminatory and extractive harms from currently deployed
 *   AI systems, with priority given to justice for marginalized populations.
 *   The constraint operates through sociotechnical audit mandates, disparate
 *   impact liability, participatory design requirements, and bias mitigation
 *   resource allocation. It extracts compliance costs from AI deployers and
 *   foundation model providers while coordinating a harm-detection
 *   infrastructure that benefits marginalized populations. The reading
 *   coexists with but structurally competes against the
 *   existential_risk_reading (which prioritizes catastrophic loss-of-control
 *   scenarios) and the integrated_reading (which treats both as
 *   complementary). This story authors ONLY the nearterm_harms_reading as a
 *   clean ε-invariant constraint per Rule 1.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.62).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.58).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment Priority: Near-Term Harm Prevention for Marginalized Populations").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, 'b88a449b-6b3d-48a8-a3ef-7fe22938ce52').
narrative_ontology:cs_kernel_codification('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', distributed).
narrative_ontology:cs_authority_grounding('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', distributed).
narrative_ontology:cs_reading_relation('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', foundational, present_harm_prevention_primacy).
narrative_ontology:cs_axiom_status(present_harm_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', present_harm_prevention_primacy, deontological).
narrative_ontology:cs_axiom('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', foundational, marginalized_justice_as_alignment_criterion).
narrative_ontology:cs_axiom_status(marginalized_justice_as_alignment_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', marginalized_justice_as_alignment_criterion, deontological).
narrative_ontology:cs_axiom('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', secondary, sociotechnical_audit_necessity).
narrative_ontology:cs_axiom_status(sociotechnical_audit_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', sociotechnical_audit_necessity, instrumental).
narrative_ontology:cs_reference_frame('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', algorithmic_accountability_movement).
narrative_ontology:cs_drift_state('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', generative_ai_deployment_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b88a449b-6b3d-48a8-a3ef-7fe22938ce52', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, civil_rights_organizations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, academic_bias_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_deployers_under_audit).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, foundation_model_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, affected_workers_and_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience discriminatory outcomes from deployed AI systems in hiring, lending, healthcare, policing, and social services. They bear the harms the constraint seeks to prevent but have no structural power to enforce audits or demand remediation. Exit from algorithmic systems is often impossible — credit scores, background checks, and benefits determinations are mandatory and opaque.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% Litigate, advocate, and organize around algorithmic discrimination. They set the policy agenda for bias audits, disparate impact standards, and participatory design requirements. They do not deploy AI systems but shape the regulatory and normative environment that constrains deployers. Their power derives from legal standing, public legitimacy, and coalition-building.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, civil_rights_organizations, agenda_setter,
    organized, generational, mobile, national).

% Develop sociotechnical audit methodologies, fairness metrics, and harm taxonomies. Their work gains legitimacy and funding when the constraint treats bias mitigation as a priority. They are not directly harmed by deployed systems but their professional standing and resource flows depend on the constraint's enforcement regime.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, academic_bias_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Corporations and agencies deploying AI in high-stakes domains (hiring, lending, healthcare, policing). They bear the cost of bias audits, model retraining, compliance infrastructure, and potential liability. They can modify systems but cannot exit the domains where audits are mandated. Some capture the audit process through voluntary standards bodies.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_deployers_under_audit, payer,
    powerful, biographical, constrained, global).

% Build and license the base models that downstream deployers fine-tune. They face pressure to implement guardrails, release model cards, and fund bias research. They have significant resources to shape standards through industry consortia and can relocate development across jurisdictions. Their dual role reflects both paying compliance costs and setting de facto technical standards.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, foundation_model_providers, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, foundation_model_providers, agenda_setter).

% Individual workers denied jobs, borrowers denied loans, patients misdiagnosed by AI systems. They experience the concrete harms the constraint addresses but have no individual recourse — litigation is prohibitively expensive, and opting out of algorithmic systems is often not an option. Their situation is the ground truth the constraint measures itself against.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, affected_workers_and_applicants, payer,
    powerless, immediate, trapped, local).

% Research catastrophic AI risk scenarios (loss of control, deception, power-seeking). They argue that prioritizing near-term bias mitigation diverts talent and funding from alignment research on advanced systems. They are excluded from the near-term harm framing's resource allocation but influence the sibling reading's agenda.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, mobile, global).

% Allocate enforcement resources, write rules, and adjudicate between competing alignment priorities. They observe both the near-term harm frame and the existential risk frame, and their decisions determine which constraint gets state backing. They do not directly bear costs or collect benefits from either reading.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, policy_makers_and_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the sociotechnical infrastructure for detecting and mitigating discriminatory harms from deployed AI: shared audit methodologies, fairness metrics, harm reporting channels, and participatory design norms. Solves the problem that no single actor can comprehensively assess system impacts across diverse populations.
% TRANSFER_FUNCTION: Moves compliance costs (audit infrastructure, model retraining, legal liability, participatory design processes) from AI deployers and foundation model providers toward bias mitigation outcomes that benefit marginalized populations. Also moves research funding and institutional attention toward sociotechnical audit methods and away from other alignment approaches.
% ABSENT_VOICES: Individual harmed persons (affected_workers_and_applicants) are structurally absent from standard-setting bodies and audit design processes. Existential risk researchers are excluded from the resource allocation this reading governs. Global South populations affected by AI deployed by Western corporations have no seat in the governance forums where this constraint's priorities are set.
% DISAPPEARANCE_RATIONALE: If the near-term harm priority vanished overnight, bias audit requirements would lapse, participatory design norms would lose regulatory force, funding for sociotechnical fairness research would contract, and deployers would revert to unmonitored deployment. Marginalized populations would lose the only structural mechanism currently requiring their harms to be measured. The world of deployed AI would rearrange toward less accountability.
% FOUNDING_PROBLEM: Deployed AI systems were producing systematic discriminatory harms against marginalized groups (racial bias in hiring algorithms, disability discrimination in benefits determination, age discrimination in lending) with no accountability mechanism. The founding problem was the absence of any structural requirement to measure, disclose, or mitigate these harms before deployment.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent investigations (ProPublica's COMPAS analysis, Gender Shades audit, NIST face recognition vendor tests), civil rights litigation (EEOC guidance on algorithmic discrimination, state law enforcement audits), and academic literature from researchers outside the AI industry (Crawford, Noble, Benjamin, Buolamwini). Industry-funded research acknowledges the problem but contests the priority ranking relative to existential risk.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects substantial compliance costs imposed on powerful institutional actors (deployers, foundation model providers) that flow toward bias mitigation — a genuine transfer, not merely coordination overhead. Suppression (0.58) captures active enforcement: audit mandates, liability regimes, and exclusion of non-compliant systems from high-stakes domains. Theater ratio (0.28) is moderate: some audit activity performs compliance without substantive harm reduction (checklist audits, gaming metrics), but the core function remains harm detection. Accessibility collapse (0.35) is low because alternative alignment framings (existential risk, integrated) remain live and resourced. Resistance (0.45) reflects industry pushback, regulatory capture of standards bodies, and jurisdictional arbitrage. The constraint is claimed as tangled_rope because it has a genuine coordination function (harm detection infrastructure) AND asymmetric extraction (deployers pay, marginalized groups benefit) requiring active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized population seat, this constraint is a rope (genuine coordination solving a real harm-detection problem with minimal coercive overhead from their perspective — they would not call audits 'coercion'). From the deployer seat, it computes as snare/tangled_rope (substantial extraction, active enforcement, constrained exit). From the foundation model provider seat, it computes as scaffold/tangled_rope hybrid (transitional compliance costs with arbitrage exit). The engine computes this per-seat divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are full beneficiaries (d near 0.0): the constraint's entire justification is their protection, they collect the harm-reduction benefit, and they are trapped in the systems the constraint governs. Civil rights organizations and academic bias researchers are beneficiaries with mobility (d ~0.15-0.25): they gain professional standing and resources but can exit to other domains. AI deployers are primary payers (d ~0.85): they bear concentrated costs, have constrained exit (cannot leave high-stakes domains), and face active enforcement. Foundation model providers are payers with arbitrage-grade exit (d ~0.65): they pay compliance costs but can relocate development and shape standards. Affected workers are identity-locked victims (d ~0.95): they experience direct harm, cannot exit algorithmic systems, and their self-concept is often fused with the domains where harm occurs (employment, credit, healthcare). Existential risk researchers are excluded (not in the constraint's beneficiary/victim structure). Policymakers are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discriminatory harms from deployed AI with no accountability) is LIVE — harms persist and evolve with new deployment domains. The constraint has not outlived its function. However, mandatrophy risk exists if the audit infrastructure becomes ritualized (theater ratio rising) while harms shift to unmonitored domains. The integrated_reading sibling attempts to capture this constraint's mandate while diluting its priority — a potential mandatrophy vector where the near-term harm frame is absorbed into a broader frame that deprioritizes its specific beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priority_crowding_out,
    'Does prioritizing near-term harm prevention structurally crowd out existential risk research, or do they draw from disjoint talent/funding pools?',
    'Track funding flows, hiring patterns, and research output across both subfields over 5+ years. Control for overall AI investment growth.',
    'If crowding out is real, the integrated_reading''s claim of complementarity is empirically false — the readings are zero-sum competitors for resources. If disjoint, the integrated_reading''s framing is structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_crowding_out, empirical, 'Whether the nearterm and existential readings compete for the same scarce resources.').

omega_variable(
    audit_effectiveness_vs_theater,
    'What fraction of mandated bias audit activity produces genuine harm reduction versus performative compliance?',
    'Longitudinal study of audit findings vs. subsequent harm metrics in deployed systems. Compare jurisdictions with strong vs. weak enforcement.',
    'If theater dominates, the constraint''s coordination function is decaying toward piton. If audits drive real mitigation, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(audit_effectiveness_vs_theater, empirical, 'The coordination/extraction boundary within the audit infrastructure itself.').

omega_variable(
    kernel_framing_contestation,
    'Is ''alignment'' a single kernel with competing readings, or are these fundamentally different concepts sharing a label?',
    'Trace the genealogy of ''alignment'' usage in technical safety vs. fairness communities. Identify whether the term''s stabilization masked a conceptual split.',
    'If different concepts, the kernel frame is a category error — each reading should be a standalone constraint with no structural relationship. If single kernel, the reading_relations and drift analysis are valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contestation, conceptual, 'Whether the ai_alignment_priority kernel is a genuine single commitment or a conflation of distinct projects.').

omega_variable(
    beneficiary_capture_of_audit_standards,
    'Do civil rights organizations and academic bias researchers (beneficiaries) capture the audit standard-setting process to entrench their methodological preferences over deployer feasibility?',
    'Analyze standard-setting body composition, comment periods, and adoption rates of competing audit frameworks.',
    'If capture exists, the constraint''s coordination function is compromised — beneficiaries become agenda_setters extracting methodological rents. This would increase theater_ratio and shift classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_audit_standards, empirical, 'Whether beneficiary groups have become de facto agenda_setters capturing the coordination mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 2018, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2018, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2018, 0.05).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2020, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2022, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2024, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2026, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2026, 0.25).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2028, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2028, 0.27).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_tr_t2030, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2030, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2018, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2020, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2022, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2024, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2026, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2026, 0.59).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2028, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2028, 0.61).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_be_t2030, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2030, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2018, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2018, 0.1).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2020, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2022, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2024, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2026, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2026, 0.53).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2028, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2028, 0.56).
narrative_ontology:measurement(ai_alignment_priority__nearterm_harms_reading_su_t2030, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2030, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.1).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, algorithmic_audit_mandate).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, disparate_impact_liability_regime).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, participatory_design_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_priority kernel. The existential_risk_reading prioritizes catastrophic loss-of-control prevention with a different victim set (humanity at civilizational scale) and different methodology (interpretability, control theory, scalable oversight). The integrated_reading attempts to combine both as complementary priorities. The three readings share the label 'alignment' but have disjoint ε referents (deployed system audits vs. future system control), disjoint victim sets, and disjoint resource flows. They are linked via network.affects_constraints because they compete for the same governance bandwidth and talent pool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, institutional, 0.65).
constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, powerless, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
