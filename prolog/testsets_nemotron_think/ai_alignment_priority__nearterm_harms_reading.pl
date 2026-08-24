% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: Near-Term AI Alignment Priority: Sociotechnical Audits and Bias Mitigation for Deployed Systems
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'near-term harms' reading of the
 *   contested AI alignment priority kernel. The reading argues that
 *   alignment's primary obligation is preventing present discriminatory and
 *   extractive harms from already-deployed AI systems, with priority given to
 *   justice for marginalized populations. The constraint is the emerging
 *   regime of mandatory sociotechnical audits, bias metrics, and mitigation
 *   requirements (EU AI Act high-risk categories, US executive orders, NIST
 *   AI RMF, state laws like CA SB 1047). It has a genuine coordination
 *   function — creating shared, contestable standards where none existed —
 *   but also substantial extraction: a fast-growing audit consulting industry
 *   captures compliance revenue, civil rights organizations gain
 *   institutional validation but face co-optation, and marginalized
 *   communities themselves are subjected to extractive consultation
 *   practices. The claimed type is tangled_rope; the metrics reflect rising
 *   extraction and theater as the regime matures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "Near-Term AI Alignment Priority: Sociotechnical Audits and Bias Mitigation for Deployed Systems").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '2655b733-ce94-45f6-a281-8e0bf518636c').
narrative_ontology:cs_kernel_codification('2655b733-ce94-45f6-a281-8e0bf518636c', distributed).
narrative_ontology:cs_authority_grounding('2655b733-ce94-45f6-a281-8e0bf518636c', practice).
narrative_ontology:cs_interpretation_layer_present('2655b733-ce94-45f6-a281-8e0bf518636c').
narrative_ontology:cs_reading_relation('2655b733-ce94-45f6-a281-8e0bf518636c', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2655b733-ce94-45f6-a281-8e0bf518636c', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('2655b733-ce94-45f6-a281-8e0bf518636c', foundational, present_harm_prevention_priority_over_speculative_risk).
narrative_ontology:cs_axiom_status(present_harm_prevention_priority_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('2655b733-ce94-45f6-a281-8e0bf518636c', present_harm_prevention_priority_over_speculative_risk, deontological).
narrative_ontology:cs_axiom('2655b733-ce94-45f6-a281-8e0bf518636c', foundational, marginalized_communities_as_epistemic_authority).
narrative_ontology:cs_axiom_status(marginalized_communities_as_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('2655b733-ce94-45f6-a281-8e0bf518636c', marginalized_communities_as_epistemic_authority, conventional).
narrative_ontology:cs_axiom('2655b733-ce94-45f6-a281-8e0bf518636c', secondary, deployed_system_accountability_as_necessary_condition).
narrative_ontology:cs_axiom_status(deployed_system_accountability_as_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('2655b733-ce94-45f6-a281-8e0bf518636c', deployed_system_accountability_as_necessary_condition, instrumental).
narrative_ontology:cs_reference_frame('2655b733-ce94-45f6-a281-8e0bf518636c', emerging_ai_governance_practice).
narrative_ontology:cs_drift_state('2655b733-ce94-45f6-a281-8e0bf518636c', post_generative_ai_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2655b733-ce94-45f6-a281-8e0bf518636c', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, civil_rights_organizations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, audit_consulting_industry).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_developers).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_populations).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, algorithmic_fairness_as_justice).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, participatory_design_epistemic_authority).
narrative_ontology:constraint_vindicates(ai_alignment_priority__nearterm_harms_reading, deployed_system_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically subjected to algorithmic discrimination (racial minorities, disabled people, elderly, low-income groups). They gain protection from discriminatory deployed systems through mandated audits and bias mitigation. However, they also bear extractive costs: uncompensated consultation labor for audits, data extraction for 'bias benchmark' datasets, and token inclusion in processes that legitimize the audit industry without transferring decision-making power. Their identity is fused with the harm the constraint addresses, making exit from the category impossible.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, marginalized_populations, payer).

% Companies and teams building deployed AI systems (foundation model providers, application developers, enterprise AI vendors). They bear substantial compliance costs: sociotechnical audit expenses, bias mitigation engineering, documentation burdens, and delayed deployments. Large labs absorb this as overhead; smaller developers face prohibitive barriers. Exit means abandoning markets with mandatory audit regimes (EU AI Act, emerging US state laws) or moving to less regulated domains — constrained by market access needs.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_developers, payer,
    powerful, biographical, constrained, global).

% Specialized firms and consultants selling sociotechnical audit services, bias benchmark datasets, fairness tooling, and compliance frameworks. They capture a growing revenue stream from mandatory audit requirements. Their expertise becomes the de facto standard for what counts as 'aligned.' They have high exit mobility — they can pivot to adjacent compliance markets — but benefit from the constraint's expansion.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, audit_consulting_industry, beneficiary,
    organized, biographical, mobile, global).

% Advocacy groups (ACLU, EPIC, Algorithmic Justice League, disability rights orgs) that pushed for deployed-system accountability. They gain institutional validation, funding streams for 'community engagement' in audits, and regulatory leverage. But they are constrained by dependence on the same funding ecosystems (foundations, government grants) that shape audit methodologies, limiting radical critique.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, civil_rights_organizations, beneficiary,
    organized, generational, constrained, national).

% Independent and community developers releasing models and tools without corporate compliance infrastructure. They face asymmetric burden: the same audit requirements apply but without legal teams or audit budgets. Many withdraw models from regulated jurisdictions or cease releases. Their exit is constrained by the global nature of model distribution — geographic gating is technically fragile.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, open_source_developers, payer,
    moderate, biographical, constrained, global).

% Government bodies (EU AI Office, FTC, state AGs, NIST) that codify audit requirements into law and enforce them. They set the agenda: which harms count, what methodologies satisfy compliance, what penalties apply. They extract institutional legitimacy and expanded authority from the constraint. Their analytical exit means they can revise requirements, but institutional inertia makes rollback unlikely.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Workers subject to algorithmic management (gig drivers, warehouse workers, call center staff) whose harms (wage theft, discriminatory scheduling, surveillance) are distinct from the 'bias' categories centered in current audits. They are not represented in audit stakeholder processes; their harms are treated as labor issues, not alignment issues. They cannot exit algorithmic management without losing livelihood.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, affected_workers, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared standards for evaluating deployed AI systems' discriminatory impacts, creating a common methodology (sociotechnical audits, disparity metrics, participatory review) where none existed — replacing ad-hoc, vendor-graded 'fairness' claims with contestable, third-party-verifiable assessment.
% TRANSFER_FUNCTION: Moves compliance costs (audit fees, engineering remediation, documentation labor) from AI developers to the audit consulting industry and civil society intermediaries; moves protective benefits (reduced discriminatory outcomes, contestability mechanisms) to marginalized populations; moves regulatory authority and legitimacy to state agencies.
% ABSENT_VOICES: Affected workers under algorithmic management (gig, logistics, care) whose harms fall outside the 'bias/disparity' frame centered in current audits. Global South communities whose data trains systems but who have no seat in Northern audit regimes. Disabled people whose access needs are reduced to 'fairness metrics' rather than design justice. They are structurally excluded by the audit methodology's categories and the policy venues where standards are set.
% DISAPPEARANCE_RATIONALE: If mandatory sociotechnical audits and bias mitigation requirements vanished overnight, foundation model providers would immediately cease voluntary fairness testing (already regressing post-2023), disparity documentation would become proprietary and unverifiable, regulatory enforcement would lose its technical basis, and the audit consulting industry would collapse — the entire accountability infrastructure for deployed AI discrimination would revert to vendor self-assessment.
% FOUNDING_PROBLEM: Pre-2020 AI deployment operated without any standardized accountability for discriminatory outcomes: hiring algorithms filtered by race proxy, medical allocation systems deprioritized Black patients, facial recognition misidentified people of color at scale — all deployed with no mandatory evaluation, no contestation mechanism, and no remedy for harmed populations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (1) investigative journalism (Angwin et al. on COMPAS, Buolamwini/Gebru on facial recognition) — outside beneficiaries; (2) congressional testimony from impacted communities (not industry); (3) academic literature documenting deployed harms (Obermeyer et al. 2019, Raji & Buolamwini 2019). The audit consulting industry and some civil rights orgs argue the problem remains live and expanding; AI developers argue the founding problem is substantially addressed by current voluntary commitments and emerging regulation — the status is genuinely contested across seats.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects the compliance cost burden on developers (especially small/open-source) and the extractive consultation practices targeting marginalized communities, relative to the coordination value of standardized audits. Suppression (0.62) captures the active enforcement machinery: mandatory conformity assessments, market withdrawal powers, penalty regimes — not merely voluntary standards. Theater ratio (0.45) is significant: performative 'ethics washing' audits, checklist compliance divorced from outcome improvement, and the growing gap between audit rituals and actual harm reduction. Accessibility collapse (0.55) and resistance (0.58) are moderate: alternatives (voluntary commitments, market pressure) persist but are weakening; resistance comes from developers (cost), open-source (exclusion), and marginalized communities (tokenization).
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/agenda_setter seat, the constraint is coordination infrastructure (rope-like). From the AI developer seat (especially small/open-source), it is extractive enforcement (snare-like). From the marginalized population seat, it is ambiguously both: genuine protection mixed with extractive consultation — the identity_locked exit means they cannot 'choose' the beneficiary framing. The audit consulting industry experiences it as pure coordination-profit (rope). The engine computes these divergences from the structural data; the claimed_type (tangled_rope) represents the author's structural judgment that no single seat's experience captures the whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are primary beneficiaries (protection from harm) but also secondary payers (extractive consultation) — identity_locked exit makes them unable to leave the harm category. AI developers are primary payers (compliance costs) with constrained exit (market access). Audit consulting industry is pure beneficiary (revenue capture) with mobile exit. Regulators are agenda_setters (institutional power, generational horizon). Civil rights organizations are beneficiaries with constrained exit (funding dependence). Open-source developers are payers with constrained exit (global distribution). Affected workers are excluded (trapped, immediate horizon) — their harms are categorically outside the audit frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unaccountable deployed discrimination) remains live and expanding with generative AI deployment. However, the constraint's mandate has already broadened from 'prevent discrimination' to 'comprehensive risk management' (EU AI Act), creating mandatrophy risk: the audit machinery now serves broader regulatory goals beyond the original justice mandate. The theater rise (0.2→0.45) signals this drift. The constraint is not yet a piton — enforcement is active, not inertial — but the extraction trajectory suggests maturation toward tangled_rope with snare characteristics for excluded seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    audit_industry_extraction_vs_coordination,
    'Does the audit consulting industry''s revenue capture exceed the coordination value of standardized sociotechnical audits, making the constraint net-extractive?',
    'Longitudinal study comparing discrimination outcomes in jurisdictions with mandatory audits vs. voluntary regimes, controlling for audit industry market concentration and consulting fees as share of compliance spend.',
    'If extraction exceeds coordination value, the constraint reclassifies toward snare for developer and marginalized community seats; if coordination dominates, tangled_rope holds. The industry''s market structure (concentrated vs. competitive) is the pivot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_industry_extraction_vs_coordination, empirical, 'Whether the audit industry is a coordination enabler or an extractive capture layer.').

omega_variable(
    marginalized_community_net_position,
    'Are marginalized communities net beneficiaries or net victims of the audit regime, given extractive consultation practices and data extraction?',
    'Participatory action research with affected communities measuring: (a) material harm reduction from deployed systems post-audit mandates; (b) uncompensated labor hours in audit processes; (c) data extraction volume for bias benchmarks; (d) decision-making power transfer vs. token consultation.',
    'If net victims, the constraint''s claimed beneficiary structure is falsified — marginalized_populations moves from beneficiary to payer in the structural map, flipping the extraction asymmetry. This would be a false summit detection for the justice framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_community_net_position, empirical, 'Whether the stated beneficiaries actually benefit net of extraction.').

omega_variable(
    kernel_framing_justice_vs_technical,
    'Is the alignment priority kernel fundamentally a justice claim (this reading) or a technical safety claim (existential_risk_reading), and does the framing determine the constraint''s type?',
    'Genealogical analysis of the term ''alignment'' in ML literature (2015-present): trace whether ''alignment'' originally meant value specification (technical) or harm prevention (justice), and how the bifurcation maps to institutional incentives (funding, regulatory capture, talent recruitment).',
    'If the kernel is inherently justice-framed, the existential_risk_reading is a reframing that displaces the original mandate (mandatrophy). If inherently technical, this reading is an expansion that creates new extraction. The framing determines which reading carries the founding problem''s corroboration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_justice_vs_technical, conceptual, 'Whether the kernel''s original commitment was justice or safety, and what that means for mandate drift.').

omega_variable(
    suppression_mechanism_regulatory_vs_market,
    'Is the constraint''s suppression primarily regulatory (state enforcement) or market-driven (procurement requirements, insurance, investor pressure)?',
    'Track compliance adoption curves: where audits are adopted without regulatory mandate (enterprise procurement, insurance requirements), measure whether suppression metrics differ from mandated jurisdictions.',
    'If market-driven suppression dominates, the constraint''s enforcement is more diffuse and harder to contest (no single regulator to petition); if regulatory, suppression is more legible but also more politically reversible. Changes the suppression ontology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_regulatory_vs_market, empirical, 'Whether suppression is state-centric or market-distributed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t2, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_a_tr_t6, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_a_be_t2, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(ai_a_be_t6, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t2, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 2, 0.38).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(ai_a_su_t6, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, eu_ai_act_high_risk_conformity).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, nist_ai_rmf_adoption).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, algorithmic_accountability_act).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_priority kernel. The existential_risk_reading (constraint_id: ai_alignment_priority__existential_risk_reading) prioritizes catastrophic loss-of-control prevention with ε ≈ 0.3 on current audits but high projected ε on future compute governance. The integrated_reading (constraint_id: ai_alignment_priority__integrated_reading) claims complementary priorities but structurally creates a unified governance regime with ε ≈ 0.55 across both horizons. All three stories link via affects_constraints. The ε-invariance principle applies: each reading has distinct ε, distinct victim/beneficiary sets, distinct coordination functions — they are separate constraints, not measurement bases of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, powerless, 0.85).
constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
