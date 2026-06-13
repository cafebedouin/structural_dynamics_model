% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Software Licensing Framework: Utilitarian Hybrid Model
 *   domain: economic/intellectual_property/software_engineering
 *
 * SUMMARY:
 *   The utilitarian-hybrid reading of the software-source-status kernel
 *   proposes that software licensing should maximize aggregate welfare, with
 *   both open-source and proprietary models legitimate when
 *   context-appropriate. This reading sits between the freedom-imperative
 *   position (which treats proprietary software as categorically unjust) and
 *   the property-rights position (which grants creators absolute rights to
 *   restrict access), and between the pragmatic-development position (which
 *   privileges open-source methodology). The reading instantiates one
 *   constraint generated from the contested kernel: software licensing should
 *   track welfare outcomes rather than ideology, with open-source as the
 *   default for infrastructure and shared tools, proprietary licensing
 *   justified only in specialized domains where it demonstrably improves
 *   outcomes. The claim/metric gap is intentional: CLAIMED as rope
 *   (coordination through shared welfare principle) while authored metrics
 *   show low-to-moderate extraction (0.38), moderate suppression (0.22), and
 *   rising resistance (0.62 endpoint), indicating that the hybrid reading
 *   faces ongoing contestation from excluded freedom-imperative and
 *   property-rights constituencies.
 *
 * KEY AGENTS:
 *   - Welfare-maximizing users (organized, mobile exit, global): benefit from both models coexisting; their welfare is the constraint's metric.
 *   - Open-source maintainers (organized, mobile exit, global): set community norms, enforce copyleft/permissive licenses; benefit from recognition that open-source has genuine quality advantages.
 *   - Proprietary software vendors (powerful, constrained exit, global): extract licensing fees; legitimate in hybrid reading only when context-specific optimization justifies enclosure; face rising resistance.
 *   - Infrastructure stakeholders (institutional, constrained exit, global): benefit from open-source default for foundational systems; have stakes in interoperability.
 *   - Specialized domain developers (moderate power, mobile exit, regional): choose licenses contextually; justified in proprietary choices under the hybrid reading when domain complexity and safety justify enclosure.
 *   - Freedom-imperative constituency (moderate power, constrained exit, excluded): structurally excluded because the reading treats freedom as context-dependent, not absolute; would argue proprietary software is inherently unjust.
 *   - Property-rights advocates (powerful, mobile exit, excluded): structurally excluded because the reading treats property rights as overridable by welfare considerations; would argue creators have unconditional rights.
 *   - Pragmatic-development constituency (moderate power, mobile exit, observer): partially aligned (open-source often superior) but departing on specialized domains; see freedom as instrumental to quality, not absolute.
 *   - Regulatory authorities (institutional, analytical exit, national): set policy on interoperability, critical infrastructure, IP protection; observe the hybrid reading as framework for principled license allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.38).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.22).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Software Licensing Framework: Utilitarian Hybrid Model").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "economic/intellectual_property/software_engineering").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '7a1cbf56-797f-4e89-8dd6-189b92ca3697').
narrative_ontology:cs_kernel_codification('7a1cbf56-797f-4e89-8dd6-189b92ca3697', distributed).
narrative_ontology:cs_authority_grounding('7a1cbf56-797f-4e89-8dd6-189b92ca3697', distributed).
narrative_ontology:cs_reading_relation('7a1cbf56-797f-4e89-8dd6-189b92ca3697', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a1cbf56-797f-4e89-8dd6-189b92ca3697', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a1cbf56-797f-4e89-8dd6-189b92ca3697', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_axiom('7a1cbf56-797f-4e89-8dd6-189b92ca3697', foundational, welfare_aggregation_principle).
narrative_ontology:cs_axiom_status(welfare_aggregation_principle, holdable).
narrative_ontology:cs_axiom_grounding('7a1cbf56-797f-4e89-8dd6-189b92ca3697', welfare_aggregation_principle, instrumental).
narrative_ontology:cs_axiom('7a1cbf56-797f-4e89-8dd6-189b92ca3697', foundational, context_dependent_optimization).
narrative_ontology:cs_axiom_status(context_dependent_optimization, holdable).
narrative_ontology:cs_axiom_grounding('7a1cbf56-797f-4e89-8dd6-189b92ca3697', context_dependent_optimization, empirically_contingent).
narrative_ontology:cs_reference_frame('7a1cbf56-797f-4e89-8dd6-189b92ca3697', rational_licensing_choice_framework).
narrative_ontology:cs_drift_state('7a1cbf56-797f-4e89-8dd6-189b92ca3697', contemporary_regulatory_pressure_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7a1cbf56-797f-4e89-8dd6-189b92ca3697', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, welfare_maximizing_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, adaptive_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, mixed_ecosystem_participants).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 endpoint) and grows slowly, then stabilizes. Early growth (0.28→0.38 over the first 25 time points) reflects proprietary vendors testing the boundaries of contextual legitimacy and establishing specialized-domain enclaves; stabilization suggests the hybrid framework has found an equilibrium where open-source dominates infrastructure and proprietary exists in justified niches. Suppression is low (0.22) but rising (0.16→0.22 over the interval) because enforcing the welfare-outcome criterion requires resisting freedom-imperative and property-rights constituencies—the suppression is resistance to excluded voices, not coercion of participants. Theater is low (0.18) and stable: the constraint does real coordination work (enabling context-dependent choice based on welfare) rather than pure performance. The measurement series is aligned on one shared grid, with observations at regular intervals (every 5 points through t=30, then a final point at t=40 to show plateau). The trajectory shows: extraction growing as proprietary vendors test boundaries, then stabilizing as specialized domains stabilize; suppression rising as excluded constituencies mount counter-arguments; theater stable because the coordination function is genuine and not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading should compute differently across seats. From the welfare-maximizing user perspective, the arrangement is genuine coordination (choices should track welfare outcomes, both models have legitimacy). From the proprietary-vendor perspective, it is partially extractive (they are constrained to justify enclosure through welfare metrics, not property rights, which is a higher bar). From the freedom-imperative perspective, it is pure extraction (the framework denies their foundational commitment and treats freedom as negotiable). From the open-source maintainer perspective, it is legitimate coordination (their quality and auditability advantages are recognized while they maintain license control). The engine should compute these as distinct classifications per seat: the constraint that is rope from one chair is snare from another because the welfare metric is not neutral—it contains contestable assumptions about what counts as welfare and who measures it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries have low directionality (d near 0.0): welfare-maximizing users benefit from having both models available and rational choice frameworks; open-source maintainers benefit from recognition of their advantages; mixed-ecosystem participants benefit from pluralism. Payers have higher directionality (d toward 0.5–1.0): proprietary vendors pay a cost (must justify enclosure through welfare outcomes, not rights), freedom-imperative and property-rights advocates pay a cost (their foundational commitments are demoted to negotiable). Excluded voices have high directionality (d near 1.0): they are structurally locked out of the decision framework because the welfare metric is constructed to override their concerns. Open-source maintainers are genuinely dual-positioned (beneficiary of the arrangement's recognition of open-source quality, but also partially paying because they must compete on welfare grounds rather than rights claims).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic—its founding problem (how to choose licenses rationally) is still live, and the arrangement continues to serve that function. The hybrid reading instantiates a solution to the founding problem, and the resistance (0.62) and rising suppression (0.16→0.22) indicate that the solution is contested but not abandoned. The founding_problem_status is correctly authored as 'contested' (pragmatic and infrastructure advocates attest the problem is solved by the hybrid approach; freedom-imperative and property-rights advocates attest the problem is solved wrongly because welfare aggregation is an invalid frame). The disappearance verdict is world_rearranges (the software ecosystem would reorganize without the constraint's decision principle), which is coherent with a live founding problem. No mandatrophy flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_demarcation_ambiguity,
    'What precise criteria distinguish contexts where open-source maximizes welfare from contexts where proprietary licensing does? Where is the boundary between ''infrastructure should be open'' and ''specialized domains may be proprietary''?',
    'Empirical analysis of welfare outcomes (security, auditability, reliability, innovation speed, accessibility) across software domains (infrastructure vs. specialized tools); regulatory mapping of where jurisdictions mandate interoperability or source disclosure; comparative cost-benefit analysis of proprietary vs. open implementations in identical domains.',
    'A clear demarcation criterion would operationalize the hybrid reading and make license choice verifiable; absence of a clear criterion leaves the reading vulnerable to rhetorical abuse (claiming ''context'' as cover for proprietary enclosure that does not maximize welfare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_demarcation_ambiguity, empirical, 'Operational demarcation of context-dependent license choice boundaries.').

omega_variable(
    welfare_metric_construction,
    'What welfare function aggregates across stakeholders (users, developers, vendors, society) and how are values measured (security, speed, cost, innovation, accessibility, equity)? Does welfare aggregation privilege majority outcomes over minority interests? Who measures welfare?',
    'Operationalization of welfare metrics through regulatory frameworks or technical standards bodies; comparative analysis of licensing outcomes using standardized welfare metrics; examination of cases where proprietary and open-source implementations compete in the same domain and welfare outcomes differ; investigation of who sets the welfare metric and whether it captures all affected parties.',
    'If welfare is measured only by corporate profit or development speed, the hybrid reading becomes cover for proprietary enclosure; if welfare includes accessibility and equity, the reading may constrain proprietary vendors more heavily. The metric construction is the constraint''s operational core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_metric_construction, conceptual, 'Welfare aggregation is constructed rather than discovered; the choice of metric shapes the licensing outcome.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the hybrid reading genuinely a middle position between freedom-imperative and property-rights extremes, or is it a subtle reframing that subordinates both to a welfare metric that serves existing power (vendors, infrastructure operators)? Does welfare aggregation neutrally optimize outcomes, or does it privilege the metrics that existing institutions can measure and control?',
    'Historical analysis of how the welfare-maximization frame has been applied in regulatory and industry contexts; examination of who benefits most when licensing is chosen by ''welfare outcomes''; comparison of outcomes under the hybrid reading vs. outcomes under freedom-imperative and property-rights readings in controlled historical or counterfactual cases; critical analysis of the welfare metric itself as a potential concealment of power.',
    'If the hybrid reading is genuinely neutral, it resolves the kernel contest by establishing welfare as arbiter and enabling context-dependent choice; if it is a subtle power move, it resolves the contest by redefining terms in a way that favors institutional actors over freedom advocates or individual creators. The reading''s legitimacy depends on whether welfare aggregation is a principle or a pretext.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Committer-frame under-determination: is the hybrid reading a genuine middle or a subtle reframing that subordinates freedom/property to a welfare metric that serves institutional power?').

omega_variable(
    proprietary_domain_innovation_empirics,
    'In domains where proprietary software dominates (medical imaging, financial modeling, aerospace systems), does proprietary licensing actually produce better welfare outcomes (safety, reliability, integration) than open-source alternatives would? Or does proprietary enclosure merely extract rent while open-source could achieve equal or better outcomes if investment were available?',
    'Comparative case studies of specialized domains where both proprietary and open-source implementations exist; analysis of welfare metrics (safety, reliability, time-to-market, cost, accessibility, interoperability) across implementations; counterfactual analysis of what would happen if proprietary domains received equivalent open-source development investment; regulatory data on actual failures and successes in proprietary vs. open systems.',
    'If proprietary licensing in specialized domains genuinely maximizes welfare, the hybrid reading is justified; if proprietary dominance is due to historical lock-in or investment disparity rather than superior outcomes, then contextual justification for proprietary licensing is weakened and the reading should be revised toward open-source default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_domain_innovation_empirics, empirical, 'Empirical welfare outcomes in proprietary-dominant domains.').

omega_variable(
    reading_coexistence_stability,
    'If the utilitarian hybrid reading coexists with freedom-imperative, property-rights, and pragmatic-development readings, under what conditions does coexistence remain stable vs. collapse into conflict? Does the hybrid reading''s framework eventually absorb or exclude the other readings?',
    'Longitudinal analysis of how the four readings have competed and coexisted historically; observation of whether the hybrid reading''s welfare criterion becomes dominant and crowds out other frameworks; examination of whether freedom-imperative and property-rights constituencies remain live positions or are gradually delegitimized as the hybrid frame becomes institutionalized.',
    'If coexistence is unstable and the hybrid reading eventually forecloses others, then the declared relation (''coexists_with'') is inaccurate and should be revised to ''influences'' or ''forecloses''. If coexistence is stable, the constraint persists as a multi-reading kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'Long-term stability of the four-reading coexistence under the software-source-status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(soft_tr_t25, software_source_status__utilitarian_hybrid_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(soft_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(soft_tr_t40, software_source_status__utilitarian_hybrid_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(soft_be_t25, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(soft_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(soft_be_t40, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.19).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(soft_su_t25, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(soft_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(soft_su_t40, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__utilitarian_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four constraint stories, each instantiating a different reading. The utilitarian-hybrid reading (this file) coexists with freedom-imperative, property-rights, and pragmatic-development readings. All four are linked via network.affects_constraints; each reading's ε value is stable within its own reading (ε-invariance), but the four readings have different ε values reflecting different foundational premises. The hybrid reading's welfare metric creates institutional pressure on all three siblings without foreclosing them logically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, powerful, 0.72).
constraint_indexing:directionality_override(software_source_status__utilitarian_hybrid_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
