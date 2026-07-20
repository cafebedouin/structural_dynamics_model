% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Software Licensing Norm
 *   domain: software engineering/political economy/intellectual property
 *
 * SUMMARY:
 *   The utilitarian hybrid reading of the software_source_status kernel holds
 *   that software licensing should maximize aggregate welfare through
 *   context-dependent optimization: open licensing for infrastructure,
 *   proprietary licensing for specialized tools. This constraint story treats
 *   the institutionalized form of that reading â as practiced by technology
 *   policy bodies, procurement standards, and foundation strategy â as a
 *   coordination mechanism that allocates licensing modes to domains. It is
 *   claimed as rope (pure coordination) because its structural operation is
 *   to solve the collective-action problem of licensing allocation without
 *   categorical victims, though the authored metrics note a modest rise in
 *   theater as the framework is adopted performatively.
 *
 * KEY AGENTS:
 *   - open_infrastructure_stewards: Primary beneficiary (moderate/mobile) â receive legitimacy and participation from the infrastructure-must-be-open rule
 *   - proprietary_specialized_vendors: Primary beneficiary (powerful/mobile) â retain proprietary licensing justification for specialized domains
 *   - technology_users: Diffuse beneficiary (organized/constrained) â access mixed ecosystem, pay for proprietary components
 *   - policy_arbiters: Agenda-setter (institutional/analytical) â apply the utilitarian calculus and set procurement terms
 *   - deontological_free_software_advocates: Excluded voice (moderate/mobile) â reject the welfare framework entirely, absent from policy tables
 *   - strong_ip_advocates: Excluded voice (moderate/mobile) â reject welfare limits on creator rights, absent from optimization forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.32).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.25).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Software Licensing Norm").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software engineering/political economy/intellectual property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '004c9d6b-bbef-453b-b450-4ec53d8ea0cb').
narrative_ontology:cs_kernel_codification('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', distributed).
narrative_ontology:cs_authority_grounding('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', expertise).
narrative_ontology:cs_interpretation_layer_present('004c9d6b-bbef-453b-b450-4ec53d8ea0cb').
narrative_ontology:cs_reading_relation('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', software_source_status__property_rights_reading, influences).
narrative_ontology:cs_axiom('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', foundational, aggregate_welfare_maximization).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization, holdable).
narrative_ontology:cs_axiom_grounding('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', aggregate_welfare_maximization, instrumental).
narrative_ontology:cs_axiom('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', foundational, contextual_licensing_optimality).
narrative_ontology:cs_axiom_status(contextual_licensing_optimality, holdable).
narrative_ontology:cs_axiom_grounding('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', contextual_licensing_optimality, empirically_contingent).
narrative_ontology:cs_reference_frame('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', welfare_maximizing_mixed_ecosystem).
narrative_ontology:cs_drift_state('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', post_infrastructure_sustainability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('004c9d6b-bbef-453b-b450-4ec53d8ea0cb', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, open_infrastructure_stewards).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, proprietary_specialized_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, technology_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain foundational software infrastructure under open licenses. They benefit from the normative priority given to open licensing for infrastructure, which directs participation, funding, and legitimacy toward their projects. Their exit consists of moving to proprietary employment or abandoning maintenance, though many remain for community commitment.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_infrastructure_stewards, beneficiary,
    moderate, generational, mobile, global).

% Develop specialized software tools under proprietary licenses, justified by the welfare framework when openness would undermine sustainability. They retain revenue from licensing and support, and the hybrid reading legitimizes their exclusionary model in domains where they claim proprietary investment maximizes welfare.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_specialized_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Access a mixed software ecosystem: open infrastructure reduces costs and improves interoperability, while proprietary specialized tools provide targeted functionality. They indirectly pay for proprietary components but are framed as net beneficiaries of the welfare-maximizing allocation.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, technology_users, beneficiary,
    organized, biographical, constrained, global).

% Government procurement offices, foundation strategy teams, and standards bodies that apply utilitarian cost-benefit analysis to decide funding and licensing requirements. They set the terms under which the hybrid framework operates and can shift the welfare calculus toward open or proprietary outcomes.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_arbiters, agenda_setter,
    institutional, generational, analytical, global).

% Assert that proprietary software is an ethical injustice regardless of welfare outcomes. They are present in public discourse but structurally excluded from policy tables where utilitarian cost-benefit analysis is the required grammar, because their deontological claims are incommensurable with the framework.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, deontological_free_software_advocates, excluded,
    moderate, generational, mobile, global).

% Assert that software creators possess absolute intellectual property rights independent of welfare calculations. Their arguments are heard in legal contexts but marginalized in technology policy forums that frame licensing as an optimization problem rather than a rights regime.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, strong_ip_advocates, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of allocating software production incentives across diverse technology domains by assigning open licensing to infrastructure (where network effects and interoperability dominate) and proprietary licensing to specialized tools (where targeted investment and differentiation dominate), preventing both underinvestment in infrastructure and underprovision of specialized innovation.
% TRANSFER_FUNCTION: Moves legitimacy and resources from monolithic licensing ideologies to context-dependent optimization; open infrastructure projects receive participation and legitimacy, proprietary specialized vendors receive justification for exclusionary licensing, and policy arbiters receive a decision heuristic.
% ABSENT_VOICES: Deontological free software advocates who reject any proprietary licensing as unethical, and strong intellectual property advocates who reject utilitarian limits on creator rights, are present in discourse but excluded from policy tables where cost-benefit analysis is the dominant grammar.
% DISAPPEARANCE_RATIONALE: If the utilitarian hybrid framework vanished, procurement standards, funding allocations, and institutional licensing decisions would lose their coordination heuristic and revert to ideological contest between pure open-source and pure proprietary mandates, disrupting current mixed-ecosystem arrangements.
% FOUNDING_PROBLEM: The software licensing wars of the 1990s and 2000s created deadweight losses: ideological gridlock prevented optimal allocation of open versus proprietary licensing, infrastructure was underfunded because open was ideologically pure but economically fragile, and specialized innovation was underprovided because proprietary was demonized regardless of context.
% FOUNDING_PROBLEM_CORROBORATION: Explicitly absent: no source entirely outside the benefiting parties corroborates the welfare-maximization framing. Free software advocates reject the problem definition as a constructed narrative to justify proprietary capture, and proprietary rights advocates reject the utilitarian solution as undermining creator rights, leaving primarily the hybrid-framework institutions to attest their own necessity.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.32) because the constraint itself is a normative heuristic, not a direct extraction mechanism; the proprietary vendors' profits are market returns, not rents delivered by the hybrid framework. Suppression is low (0.25) because the reading explicitly welcomes both licensing models and does not structurally bar alternatives. Theater rises to 0.35 because institutional adoption increasingly invokes welfare language without performing genuine cost-benefit analysis. Accessibility collapse is moderate (0.45): once the hybrid framework is accepted, pure ideological alternatives (all-open or all-proprietary) become cognitively less available in policy discourse. Resistance is moderate (0.40) from the excluded ideological camps on both flanks.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (infrastructure stewards, proprietary vendors, users) experience the constraint as solving a genuine coordination problem: without the hybrid heuristic, policy defaults to ideological gridlock. The excluded seats experience it as a suppression of their core normative claims â rights-based or freedom-based â by a framework that renders them incommensurable. The engine computes this divergence from the structural data: low power plus mobile exit for excluded voices yields a different computed type from the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared structural beneficiaries receive low directionality (subsidized by the coordination function). There are no declared victims, so the engine does not produce high-Ï targets. The excluded advocates have moderate power and mobile exit, so their derived d sits near the middle â they are not structurally trapped by this specific constraint, but they are discursively excluded from the policy venues where it operates.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists mislabeling because the coordination function is structurally live: the mixed ecosystem is not a cover story but an operational reality. There is no active enforcement requirement, no concentrated beneficiary capturing extraction, and the theater ratio, while rising, does not indicate that the constraint has atrophied into pure performance. If the founding problem (licensing gridlock) were dead and the arrangement persisted, it would drift toward piton; the contested status of the founding problem keeps the rope classification honest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_metric_incommensurability,
    'What metric of ''aggregate welfare'' governs the licensing calculus, and do different metrics (innovation rate, access equality, producer surplus) produce divergent optimal licensing mixes?',
    'Comparative institutional analysis across jurisdictions using different welfare metrics for technology policy.',
    'If welfare metrics are incommensurable, the hybrid reading cannot deliver a determinate prescription and becomes a cover for ad hoc preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_metric_incommensurability, conceptual, 'Whether welfare metrics are commensurable across licensing contexts.').

omega_variable(
    infrastructure_sustainability_empirics,
    'Does the ''open for infrastructure'' rule actually maximize welfare when accounting for maintainer burnout, funding gaps, and free-riding by proprietary specialized vendors?',
    'Longitudinal funding and sustainability data for critical open infrastructure projects under hybrid policy regimes.',
    'If open infrastructure systematically underfunds, the hybrid framework''s empirical premise is falsified and the reading collapses toward either the pragmatic or property-rights alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_sustainability_empirics, empirical, 'Whether open infrastructure sustainability validates the hybrid empirical premise.').

omega_variable(
    kernel_reading_boundary,
    'Is the utilitarian hybrid reading a distinct constraint or merely a composite of the pragmatic development and property rights readings?',
    'Track whether hybrid-framework institutions produce prescriptions that differ from what a pragmatic reading or property-rights reading would separately recommend.',
    'If the reading is not independently action-guiding, its epsilon should be distributed to its component readings rather than authored separately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this reading is structurally distinct or a composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t4, software_source_status__utilitarian_hybrid_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(soft_tr_t8, software_source_status__utilitarian_hybrid_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(soft_tr_t12, software_source_status__utilitarian_hybrid_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(soft_tr_t16, software_source_status__utilitarian_hybrid_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(soft_tr_t24, software_source_status__utilitarian_hybrid_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(soft_be_t4, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(soft_be_t8, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(soft_be_t12, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(soft_be_t16, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(soft_be_t24, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 24, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(soft_su_t4, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(soft_su_t8, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 8, 0.15).
narrative_ontology:measurement(soft_su_t12, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement(soft_su_t16, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(soft_su_t24, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 24, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four structurally distinct readings because the natural-language label 'software source status' conflates deontological, pragmatic, rights-based, and utilitarian claims. Each reading has a distinct epsilon, beneficiary structure, and type. They are linked as a constraint family via lateral institutional coupling rather than causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
