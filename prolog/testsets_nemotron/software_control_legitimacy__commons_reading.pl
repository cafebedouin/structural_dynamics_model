% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The commons reading of software control legitimacy positions digital
 *   infrastructure as a common-pool resource requiring multi-stakeholder
 *   governance. It rejects both the proprietary model (exclusive control as
 *   property right) and the free software absolutist model (freedom as
 *   non-negotiable ethical imperative) in favor of negotiated collective
 *   management. The constraint is the governance framework itself — the
 *   rules, norms, and institutions that allocate authority over shared
 *   software. This reading instantiates one constraint from the contested
 *   kernel 'software_control_legitimacy'; sibling readings instantiate
 *   different constraints with different beneficiary/victim structures and
 *   extraction profiles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.22).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.18).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba').
narrative_ontology:cs_kernel_codification('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', distributed).
narrative_ontology:cs_authority_grounding('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', practice).
narrative_ontology:cs_interpretation_layer_present('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba').
narrative_ontology:cs_reading_relation('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', software_control_legitimacy__property_rights_reading, influences).
narrative_ontology:cs_axiom('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', foundational, software_as_common_pool_resource).
narrative_ontology:cs_axiom_status(software_as_common_pool_resource, holdable).
narrative_ontology:cs_axiom_grounding('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', software_as_common_pool_resource, empirically_contingent).
narrative_ontology:cs_axiom('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', foundational, governance_by_affected_stakeholders).
narrative_ontology:cs_axiom_status(governance_by_affected_stakeholders, holdable).
narrative_ontology:cs_axiom_grounding('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', governance_by_affected_stakeholders, deontological).
narrative_ontology:cs_reference_frame('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', ostrom_commons_principles_applied_to_digital_infrastructure).
narrative_ontology:cs_drift_state('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', contemporary_platform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9332cf9-6a76-4bb4-a0b2-f3b845ee68ba', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, open_source_maintainers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, user_collectives).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, hardline_free_software_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, platform_operators).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, commons_governance_principle).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, negotiated_collective_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities of users, developers, and organizations that depend on shared digital infrastructure. They gain governance voice and shared stewardship through the commons framework. Their exit is constrained by network effects and dependency on the infrastructure they help govern.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, biographical, constrained, global).

% Developers and maintainers who steward core infrastructure projects. They set governance norms through contribution practices and community processes. They can fork or migrate projects, but network effects and contributor communities constrain easy exit.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, open_source_maintainers, agenda_setter,
    moderate, biographical, mobile, global).

% Organized user groups (cooperatives, nonprofits, public institutions) that depend on software infrastructure. They gain collective bargaining power in governance but face switching costs from entrenched dependencies.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, user_collectives, beneficiary,
    moderate, biographical, constrained, global).

% Commercial software vendors whose business models rely on exclusive control. The commons framework denies them unilateral authority over licensing and distribution terms. They bear compliance costs and competitive pressure from commons-based alternatives. Exit means abandoning proprietary control — structurally constrained by their business model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_vendors, payer,
    powerful, generational, constrained, global).

% Advocates who view any compromise on absolute user freedom as ethical failure. The commons reading's negotiated governance denies them the absolutist framing they treat as non-negotiable. Their identity is fused to the freedom imperative — exit from the position is identity-threatening. They bear the cost of seeing their ethical framework treated as one voice among many.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, hardline_free_software_advocates, payer,
    organized, civilizational, identity_locked, global).

% Major platform operators (cloud providers, app store operators, OS vendors) that host and distribute software. They both set governance terms for their platforms and pay compliance costs when commons governance conflicts with their control. Their scale gives them agenda-setting power but also makes them targets of collective governance demands.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, platform_operators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, platform_operators, payer).

% Legislators and regulators shaping digital governance frameworks. They observe the commons model as a policy alternative to both proprietary monopolies and unregulated openness. Their analytical seat carries formal authority to encode commons principles into law.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% Scholars of commons governance, software studies, and digital political economy. They provide the empirical and theoretical foundation for the commons reading but hold no direct governance power over infrastructure.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, academic_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Govern shared digital infrastructure as a common-pool resource: align incentives across diverse stakeholders, prevent enclosure by any single party, sustain maintenance through collective contribution, and allocate decision-making authority proportionally to dependence and contribution.
% TRANSFER_FUNCTION: Moves governance authority from unilateral controllers (proprietary vendors, platform operators) to multi-stakeholder processes. Moves maintenance labor from volunteer/coerced to recognized collective obligation. Moves value capture from exclusive rents to shared benefit streams.
% ABSENT_VOICES: End users without organizational representation; Global South communities dependent on infrastructure governed elsewhere; future generations who will inherit governance structures; small-scale developers excluded from governance processes by participation barriers.
% DISAPPEARANCE_RATIONALE: If the commons governance framework vanished, proprietary vendors would reassert unilateral control over key infrastructure, platform operators would impose take-it-or-leave-it terms, and the collective stewardship mechanisms sustaining critical software would collapse — the digital infrastructure landscape would reorganize around exclusive control.
% FOUNDING_PROBLEM: The tragedy of the anticommons in software: overlapping exclusive rights (patents, copyrights, trade secrets, EULAs) fragmented the knowledge commons, blocked interoperability, and made collective maintenance of shared infrastructure legally precarious. Proprietary enclosure and absolutist copyleft both failed to provide stable governance for interdependent digital infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Ostrom's commons governance principles (Nobel 2009) applied to digital infrastructure by Hess & Ostrom (2007); empirical studies of successful open infrastructure governance (Linux Foundation, Apache, Kubernetes, Python) by Weber (2004), Benkler (2006), and recent CNCF governance analyses confirm the problem persists and commons models show measurable success. Proprietary vendors and hardline freedom advocates both dispute the framing but cannot claim the problem is solved.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and declining (0.35→0.22) as commons governance matures and reduces rent extraction by any single party. Suppression is low (0.25→0.18) because the framework relies on voluntary participation and reputational enforcement, not coercion. Theater ratio is low and declining (0.25→0.12) as performative 'open governance' rituals are replaced by functional multi-stakeholder processes. The commons reading claims rope type — genuine coordination with net benefits — while the metrics show declining extraction consistent with improving coordination. The claimed type and metrics are authored independently; the engine will compute per-seat classifications.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (proprietary vendors, hardline advocates) experience this constraint as extraction — their preferred control models are displaced. The beneficiary seats (communities, maintainers, users) experience it as coordination — they gain governance they lacked. The engine computes this divergence from the structural data. The commons reading's legitimacy depends on this divergence being real and acknowledged, not suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities, maintainers, and user collectives are beneficiaries (d low) — they gain governance voice and shared stewardship. Proprietary vendors are payers (d high) — they lose unilateral control and bear compliance costs. Hardline free software advocates are payers with identity_locked exit (d very high) — their ethical framework is structurally denied absolutist status. Platform operators are dual-positioned: agenda_setters on their platforms but payers when commons governance constrains them. Policy makers and researchers are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anticommons fragmentation) remains live — interdependence of digital infrastructure has only increased. The commons framework is not a vestigial arrangement; it actively solves a growing coordination problem. No mandatrophy: the constraint's function has expanded with the scope of shared infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_rule_design_variance,
    'How much does the commons reading''s extraction profile (ε) vary depending on specific governance rule designs (e.g., copyleft vs permissive licensing, foundation vs BDFL governance, corporate vs community control)?',
    'Comparative case studies of commons-governed projects with different rule sets, measuring rent extraction, contributor retention, and fork rates.',
    'If ε varies widely, the commons reading is not a single constraint but a family — each rule set instantiates a different constraint. If ε is stable, the commons principle itself constrains extraction regardless of rule details.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_rule_design_variance, empirical, 'Whether the commons reading''s ε is invariant across governance implementations or rule-dependent').

omega_variable(
    absolutist_victim_status,
    'Are proprietary vendors and hardline free software advocates genuinely ''victims'' of the commons reading, or do they merely lose a contested political struggle?',
    'Analyze whether the commons framework denies them participation rights that a just governance system would grant, or merely denies them the unilateral control they claim as right.',
    'If they are genuine victims, the commons reading has an asymmetric extraction component (tangled_rope risk). If they are merely political losers, the reading is closer to pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutist_victim_status, conceptual, 'Whether absolutist positions denied governance voice count as structural victims').

omega_variable(
    kernel_reading_decomposition,
    'Does the kernel ''software_control_legitimacy'' decompose into four cleanly separable constraints, or do the readings share structural components that make ε non-invariant across readings?',
    'Apply the ε-invariance test: for each reading, identify the standing arrangement under contest and assess whether changing the observable (freedom metric, property metric, commons metric) changes ε for that reading.',
    'If readings share structural components, the kernel is a single constraint with observer-dependent classification. If they are separable, each reading is a distinct constraint story linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the four readings satisfy the ε-invariance principle for constraint decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__commons_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__commons_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__commons_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__commons_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__commons_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__commons_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__commons_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__commons_reading, base_extractiveness, 40, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__commons_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__commons_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__commons_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__commons_reading, suppression_requirement, 32, 0.16).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__commons_reading, suppression_requirement, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This commons_reading decomposes the kernel 'software_control_legitimacy' alongside three sibling readings. The freedom_imperative_reading treats proprietary control as ethical violation (high extraction on users). The pragmatic_openness_reading treats openness as quality methodology (low extraction, coordination-focused). The property_rights_reading treats control as legitimate property (extraction on users as justified return). This reading treats control as commons governance (extraction minimized by collective rules). Each reading has distinct ε, beneficiaries, victims, and claimed types — they are separate constraints linked by the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, organized, 0.15).
constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, powerful, 0.85).
constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
