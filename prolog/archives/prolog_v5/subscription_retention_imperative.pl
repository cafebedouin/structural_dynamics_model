% ============================================================================
% CONSTRAINT STORY: subscription_retention_imperative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_retention_imperative, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: subscription_retention_imperative
 *   human_readable: Subscription Retention Imperative in Digital Media
 *   domain: media_studies/political_economy/cognitive_infrastructure
 *
 * SUMMARY:
 *   The subscription retention imperative represents a structural constraint
 *   emerging from the mathematics of recurring revenue models. Media
 *   organizations operating on subscription economics face an immutable
 *   tradeoff: content optimized for mobilization (urgent, disruptive,
 *   action-oriented) produces high churn, while content optimized for
 *   retention (stable, predictable, habit-forming) produces sustainable
 *   revenue. The 42% conversion rate within 6 months and ARPU growth from
 *   tenured cohorts ($9.72 Q4 2025) reflect this structural reality. However,
 *   the constraint's classification as mountain depends on whether this
 *   tradeoff is inherent to sustainable journalism or contingent on the
 *   subscription business model itself. The presence of identifiable
 *   beneficiaries (media organizations and tenured subscribers who prefer
 *   retention-optimized content) triggers false summit detection, requiring
 *   resolution of whether alternative models could achieve sustainability
 *   without the retention imperative.
 *
 * KEY AGENTS:
 *   - Media Organizations: Primary beneficiary (institutional/arbitrage) — subscription model provides revenue stability; retention optimization is experienced as natural business necessity
 *   - Tenured Subscriber Cohorts: Secondary beneficiary (moderate/mobile) — long-term subscribers prefer stable, predictable content that justifies ongoing payment; ARPU growth reflects genuine value alignment
 *   - Casual Readers: Neutral observer (moderate/mobile) — non-subscribers experience retention optimization indirectly through content selection; 42% conversion rate suggests filter rather than trap
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees retention imperative as structural property of recurring revenue models across all domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_retention_imperative, 0.08).
domain_priors:suppression_score(subscription_retention_imperative, 0.03).
domain_priors:theater_ratio(subscription_retention_imperative, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_retention_imperative, extractiveness, 0.08).
narrative_ontology:constraint_metric(subscription_retention_imperative, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(subscription_retention_imperative, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(subscription_retention_imperative, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(subscription_retention_imperative, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_retention_imperative, mountain).
narrative_ontology:human_readable(subscription_retention_imperative, "Subscription Retention Imperative in Digital Media").
narrative_ontology:topic_domain(subscription_retention_imperative, "media_studies/political_economy/cognitive_infrastructure").

domain_priors:emerges_naturally(subscription_retention_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_retention_imperative, media_organizations).
narrative_ontology:constraint_beneficiary(subscription_retention_imperative, tenured_subscriber_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIA ORGANIZATION (MOUNTAIN) — Subscription economics impose an immutable constraint: recurring revenue requires stable returning readers. Content optimization for retention is not a choice but a structural necessity of the business model. The organization experiences this as a natural law of digital media economics.
constraint_indexing:constraint_classification(subscription_retention_imperative, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: TENURED SUBSCRIBER (MOUNTAIN) — Long-term subscribers experience content optimization for retention as a natural feature of subscription media. The constraint is invisible because it aligns with their preferences: they want stable, predictable content that justifies ongoing payment. ARPU growth from tenured cohorts ($9.72 Q4 2025) reflects genuine value alignment, not extraction.
constraint_indexing:constraint_classification(subscription_retention_imperative, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CASUAL READER (MOUNTAIN) — Non-subscribers experience the retention optimization indirectly through content selection and framing. The 42% conversion rate within 6 months suggests the constraint operates as a filter rather than a trap: readers who don't align with retention-optimized content simply don't subscribe. No coercion, no suppression.
constraint_indexing:constraint_classification(subscription_retention_imperative, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the subscription retention imperative is a structural property of recurring revenue models across all domains. Any business model requiring stable returning customers will optimize for retention over mobilization. This is not specific to media — it applies to SaaS, gyms, utilities, and any subscription service. The constraint emerges from the mathematics of customer lifetime value, not from institutional choice.
constraint_indexing:constraint_classification(subscription_retention_imperative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_retention_imperative_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(subscription_retention_imperative, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_retention_imperative, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(subscription_retention_imperative, ExtMetricName, E),
    domain_priors:suppression_score(subscription_retention_imperative, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(subscription_retention_imperative),
    narrative_ontology:constraint_metric(subscription_retention_imperative, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(subscription_retention_imperative, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(subscription_retention_imperative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal extraction because it operates through preference alignment rather than coercion. Tenured subscribers who generate ARPU growth are receiving content they value; casual readers who don't convert simply exit without cost. The retention optimization creates a selection effect (filtering for readers who prefer stable content) rather than an extraction mechanism. Suppression (0.03): Negligible. No barriers prevent readers from exiting subscriptions or accessing alternative media. The 42% conversion rate indicates the constraint operates through voluntary alignment, not through suppression of alternatives. Theater ratio (0.15): Very low. Subscription metrics (conversion rate, churn by content type, ARPU growth) are genuine functional measurements, not performative proxies. Media organizations track these metrics because they directly determine revenue sustainability. Accessibility collapse (0.92): Very high. The retention imperative is accessible to all observers — media organizations, subscribers, and analysts all recognize that recurring revenue requires stable returning customers. Resistance (0.08): Very low. The constraint shows minimal resistance to analytical scrutiny. The mathematics of customer lifetime value are well-understood and non-controversial.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain, reflecting the hypothesis that the retention imperative is a structural property of subscription economics. The institutional perspective (media organization) experiences it as a business necessity. The moderate perspectives (tenured subscriber and casual reader) experience it as a natural feature of subscription media or a voluntary filter. The analytical perspective sees it as a universal property of recurring revenue models. The absence of perspectival gap is itself diagnostic: if the constraint is a genuine mountain, all observers should converge on the same classification. If it's a false summit, the convergence reveals successful naturalization of a contingent arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Media organizations are declared as beneficiaries because they achieve revenue stability through the retention imperative. Tenured subscriber cohorts are declared as beneficiaries because retention-optimized content aligns with their preferences (stable, predictable, habit-forming content that justifies ongoing payment). However, this beneficiary declaration triggers false summit detection: if the constraint is a genuine natural law (subscription economics inherently require retention optimization), then beneficiaries are simply agents whose preferences align with an immutable constraint. If the constraint is contingent (alternative business models could achieve sustainability without retention optimization), then beneficiaries are agents who benefit from naturalizing a constructed constraint. The omega variables address this ambiguity directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that very low extraction (0.08) can coexist with identifiable beneficiaries when the constraint operates through preference alignment rather than coercion. The retention imperative is not extractive because it filters for subscribers whose preferences align with retention-optimized content, rather than trapping subscribers who prefer mobilization-oriented content. The 42% conversion rate and low churn among tenured cohorts indicate voluntary alignment, not extraction. However, the false summit detector will flag this constraint for review: are media organizations and tenured subscribers benefiting from a natural law, or from a constructed constraint that naturalizes their preferences? The omega variables provide the resolution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retention_mobilization_tradeoff,
    'Is the retention-mobilization tradeoff a genuine structural constraint of subscription economics, or a contingent choice that could be resolved through alternative business models?',
    'Comparative analysis of hybrid models (e.g., impact-funded journalism, cooperative ownership, public media) that attempt to decouple revenue stability from content optimization for retention. Measure whether these models achieve comparable financial sustainability without the retention imperative.',
    'If alternative models achieve sustainability without retention optimization: the constraint is contingent (false summit). If all sustainable models converge on retention optimization: the constraint is structural (genuine mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_mobilization_tradeoff, empirical, 'Whether retention-mobilization tradeoff is structural or contingent').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are media organizations and tenured subscribers genuine beneficiaries of a natural constraint, or are they beneficiaries of a constructed constraint that naturalizes their preferences?',
    'Historical analysis of media business models pre-subscription era. Did advertising-supported models produce different content optimization patterns? Do public media models without subscription imperatives produce different mobilization vs retention balances?',
    'If subscription model produces unique retention bias not present in alternative models: beneficiaries are benefiting from a constructed constraint (false summit). If all sustainable models converge on similar retention optimization: beneficiaries are simply aligned with a natural constraint (genuine mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether beneficiaries indicate constructed vs natural constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_retention_imperative, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subret_tr_t0, subscription_retention_imperative, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subret_tr_t3, subscription_retention_imperative, theater_ratio, 3, 0.14).
narrative_ontology:measurement(subret_tr_t6, subscription_retention_imperative, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(subret_be_t0, subscription_retention_imperative, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(subret_be_t3, subscription_retention_imperative, base_extractiveness, 3, 0.07).
narrative_ontology:measurement(subret_be_t6, subscription_retention_imperative, base_extractiveness, 6, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_retention_imperative, resource_allocation).

% DUAL FORMULATION NOTE:
% The subscription retention imperative is a candidate for decomposition if alternative business models (advertising-supported, impact-funded, cooperative) produce structurally different retention-mobilization tradeoffs. If all sustainable models converge on similar retention optimization, the constraint is a genuine mountain. If alternative models achieve sustainability without retention optimization, the subscription-specific constraint should be decomposed from the general constraint of sustainable journalism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
