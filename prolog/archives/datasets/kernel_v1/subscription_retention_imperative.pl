% ============================================================================
% CONSTRAINT STORY: subscription_retention_imperative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   emerging from the mathematics of recurring revenue models in digital
 *   media. Media organizations operating on subscription economics face a
 *   fundamental design problem: content optimized for mobilization (urgent,
 *   surprising, action-oriented) produces high cancellation rates, while
 *   content optimized for retention (stable, predictable, habit-forming,
 *   emotionally consistent) sustains subscriber bases. This constraint
 *   appears as a natural law of economics but exhibits all the markers of an
 *   institutional extraction mechanism masquerading as mathematical
 *   necessity. The constraint operates across the entire cognitive
 *   infrastructure of digital news — affecting which stories are covered, how
 *   they are framed, what investigative resources are allocated, and which
 *   audiences receive which information. It creates asymmetric extraction:
 *   publishers benefit from subscription revenue stabilization; subscribers
 *   are trapped in habit-forming interfaces designed to minimize
 *   cancellation; journalists face assignment pressure toward
 *   retention-optimized angles; and the civic information ecosystem
 *   experiences systematic bias toward engagement-maximizing content over
 *   public-knowledge-maximizing content.
 *
 * KEY AGENTS:
 *   - Subscription-Dependent Publisher: Primary beneficiary (institutional/arbitrage) — captures recurring revenue, predictable cash flow, and data about subscriber behavior; insulated from advertising market volatility
 *   - Habituated Subscriber: Primary victim (powerless/trapped) — trapped in renewal cycle with high friction cancellation; psychological switching costs; continues payment despite dissatisfaction
 *   - Epistemically Vulnerable Audience: Secondary victim (powerless/trapped) — depends on media for civic knowledge; exposed to content optimized for retention over accuracy; epistemically trapped in habit-forming feeds
 *   - Staff Journalist: Tertiary actor (moderate/constrained) — experiences coordination (covering events audiences engage with) alongside extraction (assignment pressure toward retention angles, suppression of coverage that would cause churn)
 *   - Civic Information Ecosystem: Organizational victim (organized/constrained) — experiences coordination (subscription model stabilizes local reporting funding) alongside extraction (systematic bias away from investigative and beat reporting)
 *   - Platform Intermediary: Secondary beneficiary (powerful/arbitrage) — coordinates distribution while extracting value through algorithmic curation that reinforces retention-optimized content; profits from both subscription model and its externalities
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent editorial choices as immutable mathematical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_retention_imperative, 0.58).
domain_priors:suppression_score(subscription_retention_imperative, 0.65).
domain_priors:theater_ratio(subscription_retention_imperative, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_retention_imperative, extractiveness, 0.58).
narrative_ontology:constraint_metric(subscription_retention_imperative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(subscription_retention_imperative, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_retention_imperative, tangled_rope).
narrative_ontology:human_readable(subscription_retention_imperative, "Subscription Retention Imperative in Digital Media").
narrative_ontology:topic_domain(subscription_retention_imperative, "media_studies/political_economy/cognitive_infrastructure").

domain_priors:requires_active_enforcement(subscription_retention_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_retention_imperative, subscription_dependent_publishers).
narrative_ontology:constraint_victim(subscription_retention_imperative, editorial_independence).
narrative_ontology:constraint_victim(subscription_retention_imperative, audience_epistemic_wellbeing).
narrative_ontology:constraint_victim(subscription_retention_imperative, civic_mobilization_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HABITUATED SUBSCRIBER (SNARE) — Trapped in a renewal cycle designed to minimize cancellation friction. High cognitive switching costs, platform lock-in, and habit automation mean the subscriber cannot meaningfully exit despite dissatisfaction. The system extracts continuous payment through inertia, not choice.
constraint_indexing:constraint_classification(subscription_retention_imperative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EPISTEMICALLY VULNERABLE AUDIENCE (SNARE) — Those most dependent on media for civic knowledge face optimization for retention over accuracy. Content design prioritizes keeping attention and subscriptions over informing. Audiences without alternative epistemic resources are trapped in a feed designed to maximize psychological engagement rather than comprehension.
constraint_indexing:constraint_classification(subscription_retention_imperative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STAFF JOURNALIST (TANGLED ROPE) — Experiences genuine coordination (covering events readers care about, building audience) alongside asymmetric extraction (assignment pressure toward retention-optimized angles, suppression of coverage that would cause cancellations). Career security depends on subscriber metrics, not editorial judgment.
constraint_indexing:constraint_classification(subscription_retention_imperative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIC INFORMATION ECOSYSTEM (TANGLED ROPE) — Organized actors (news organizations, civil society) experience coordination (media funding via subscriptions stabilizes local reporting) alongside extraction (systematic bias toward content that drives subscriptions over content that serves civic knowledge, hollowing out investigative and beat reporting).
constraint_indexing:constraint_classification(subscription_retention_imperative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUBSCRIPTION-DEPENDENT PUBLISHER (ROPE) — Benefits from the retention mechanism as coordination: subscriber revenue enables continued operation. The constraint is experienced as a beneficial coordination solution to funding scarcity — publishers solve the problem of sustainable media economics through retention design. Net beneficiary.
constraint_indexing:constraint_classification(subscription_retention_imperative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM INTERMEDIARY (TANGLED ROPE) — Coordinates media distribution and reader identity (arbitrage exit via advertising, data sales) while extracting value through algorithmic curation that reinforces retention-optimized content. Benefits from the subscription model's success while also profiting from its externalities.
constraint_indexing:constraint_classification(subscription_retention_imperative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the retention-mobilization tradeoff reflects an immutable mathematical constraint: recurring revenue models inherently favor habit and predictability over disruption. This appears as a law of economics. However, structural beneficiaries exist — the engine will compute false summit, revealing that the 'inherent to subscription math' framing naturalizes a contingent institutional choice to maximize extraction over editorial freedom.
constraint_indexing:constraint_classification(subscription_retention_imperative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_retention_imperative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_retention_imperative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_retention_imperative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_retention_imperative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_retention_imperative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, trending upward. The constraint operates through direct extraction (subscribers trapped in renewal cycles), epistemic extraction (audience fed retention-optimized rather than knowledge-optimized content), and editorial extraction (journalists assigned toward retention angles). The trend from 0.32 to 0.58 reflects intensifying optimization pressure as competitive pressures in the subscription market increase. The constraint is not extracting maximally because alternative funding models and editorial independence choices are structurally possible — publishers have chosen this path, not been forced into it. Suppression (0.65): High and rising. Suppression operates through multiple mechanisms: (1) behavioral design of cancellation interfaces (technical suppression of exit), (2) habit automation and psychological switching costs (cognitive suppression), (3) epistemic suppression — audiences receive systematically biased information designed to maximize engagement rather than comprehension, (4) editorial suppression — journalists operate under assignment pressure toward retention-optimized angles. The rising trajectory reflects intensifying suppression investments as subscription markets saturate. Theater ratio (0.48): Moderate and stable. Retention optimization does produce genuine engagement metrics and subscriber behavior — it is not purely performative. However, engagement metrics increasingly decouple from information quality or civic outcomes. The theater represents the gap between metrics-driven content design and editorial purpose.
 *
 * PERSPECTIVAL GAP:
 *   The structure gap is stark: beneficiary publishers perceive coordination (rope), powerless subscribers perceive extraction (snare), moderate journalists perceive hybrid (tangled rope), and the civilizational analytical observer risks naturalizing extraction as law (false mountain). The gap reveals how the same structural constraint — retention-optimized content design — produces radically different classifications depending on power, exit options, and benefit flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the beneficiary/victim structure: subscription publishers are primary beneficiaries (d ≈ 0.15, low extraction experienced); subscribers are victims (d ≈ 0.85, high extraction experienced); journalists and civic actors are mixed (d ≈ 0.55-0.65). The epistemic dimension (platform curation, information quality trade-offs) is captured in the omega variables rather than in directionality itself — directionality measures economic/material extraction, while the omegas address epistemic suppression mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the classic mandatrophy pattern: the beneficiary sees pure coordination (rope), the victims see pure extraction (snare), and the analytical observer at civilizational scope risks false summit (natural law). Resolution requires recognizing that all three classifications are structurally correct relative to their observation points, but the beneficiary's rope experience masks the victims' snare experience. The constraint resolves the mandatrophy by showing that subscriber retention IS a coordination mechanism (solving the problem of sustainable media funding) AND an extraction mechanism (optimizing content for habit-formation over knowledge). Both are true. The ethical and political question is whether the coordination benefit justifies the extraction cost — that is not resolved by classification, but made visible by it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mobilization_vs_retention_optimization,
    'Is the retention-mobilization tradeoff truly inherent to subscription economics, or is it a contingent choice by publishers to optimize for maximum extraction over editorial independence?',
    'Comparative analysis of publisher editorial policies across different revenue models (subscription-primary, advertising-primary, membership, grants); measurement of editorial constraint elasticity with revenue model; historical case studies of publishers that prioritized editorial independence despite subscription pressure (and their outcomes)',
    'If inherent: mountain classification is correct — subscription economics mathematically forces retention optimization. If contingent: false summit — publishers chose extraction over independence; alternative editorial models are structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_vs_retention_optimization, empirical, 'Whether retention-mobilization tradeoff is inherent to subscription math or a contingent editorial choice').

omega_variable(
    cancellation_friction_necessity,
    'How much of the observed subscriber retention derives from genuinely reduced cancellation friction (behavioral design, habit automation) versus from content quality and audience satisfaction?',
    'Experimental variation: A/B testing cancellation interfaces (friction vs frictionless); longitudinal analysis of churn rates pre- and post-interface redesign; comparison of churn elasticity across publishers with different friction levels but similar content quality',
    'If friction-driven: suppression mechanism is the design of the cancellation path itself — high suppression score is correct. If satisfaction-driven: retention reflects genuine audience preference — suppression is lower, constraint approaches rope rather than snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cancellation_friction_necessity, empirical, 'Relative contributions of behavioral friction versus content satisfaction to retention').

omega_variable(
    editorial_independence_cost_empirical,
    'What is the empirical cost (in subscriber churn and revenue) of publishing coverage that does NOT optimize for retention?',
    'Measurement of subscriber response (churn, engagement) to specific editorial decisions: investigations that anger advertisers or subscribers; coverage of issues that depress engagement; long-form journalism that demands sustained attention; coverage of topics with low sustained reader interest; cross-publisher correlation between editorial constraint and financial performance',
    'If cost is severe: publishers face genuine economic pressure — extraction is real but bounded by economic necessity, not pure rent-seeking. If cost is moderate: publishers could fund editorial independence through mixed revenue strategies — the constraint reflects choice, not necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editorial_independence_cost_empirical, empirical, 'Financial impact of editorial independence from retention optimization').

omega_variable(
    audience_habitual_vs_informed_engagement,
    'Does subscription retention design produce primarily habitual engagement (scrolling, skim-reading, viral sharing) or genuinely informed engagement (deep reading, knowledge retention, behavior change)?',
    'Measurement of reading depth, comprehension, and behavior change across retention-optimized versus editorial-priority content; longitudinal tracking of subscriber knowledge acquisition and civic participation; comparison of engagement metrics versus cognitive outcome metrics',
    'If primarily habitual: suppression is high (audience is trapped in low-information engagement) and theater_ratio is high (metrics-driven content mimics information without substance). If substantial informed engagement: suppression is lower, audience is not epistemically trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_habitual_vs_informed_engagement, empirical, 'Whether subscription retention produces habitual versus informed audience engagement').

omega_variable(
    alternative_funding_model_feasibility,
    'Are hybrid or alternative funding models (membership, public funding, grants, cooperative ownership) structurally capable of funding news organizations at current scale while preserving editorial independence?',
    'Case study analysis of news organizations with non-subscription-primary funding; financial comparison of revenue-per-journalist and editorial autonomy across funding models; assessment of scalability of alternative models to support investigative and beat reporting',
    'If feasible: the constraint reflects choice to extract maximum subscriber value rather than necessity — Snare classification more defensible. If infeasible: publishers face genuine economic pressure — constraint reflects real structural bind between sustainability and editorial freedom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_model_feasibility, empirical, 'Feasibility of alternative funding models for news organizations').

omega_variable(
    false_summit_natural_law_framing,
    'Does the ''subscription mathematics as natural law'' framing serve institutional interests by naturalizing what is actually a contingent design choice?',
    'Rhetorical analysis of publisher statements about retention constraints; historical comparison of editorial freedom across subscription adoption periods; identification of moments where publishers chose editorial independence despite subscription pressure (and documented decision-making rationale)',
    'If framing serves institutional extraction: mountain classification is false summit — reveals how mathematical determinism rhetoric protects extractive design from scrutiny. If framing reflects genuine structural constraint: mountain classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'Whether natural law framing serves to legitimize contingent extraction choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_retention_imperative, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_ret_tr_t0, subscription_retention_imperative, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subs_ret_tr_t4, subscription_retention_imperative, theater_ratio, 4, 0.42).
narrative_ontology:measurement(subs_ret_tr_t8, subscription_retention_imperative, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(subs_ret_be_t0, subscription_retention_imperative, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(subs_ret_be_t4, subscription_retention_imperative, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(subs_ret_be_t8, subscription_retention_imperative, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_ret_su_t0, subscription_retention_imperative, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_ret_su_t4, subscription_retention_imperative, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(subs_ret_su_t8, subscription_retention_imperative, suppression_requirement, 8, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_retention_imperative, resource_allocation).
narrative_ontology:affects_constraint(subscription_retention_imperative, algorithmic_curation_bias).
narrative_ontology:affects_constraint(subscription_retention_imperative, editorial_independence_degradation).
narrative_ontology:affects_constraint(subscription_retention_imperative, civic_information_poverty).
narrative_ontology:affects_constraint(subscription_retention_imperative, subscriber_surveillance_infrastructure).

% DUAL FORMULATION NOTE:
% The subscription retention imperative is upstream of multiple downstream constraints: algorithmic curation that reinforces retention optimization, editorial independence erosion as publishers optimize for metrics, civic information poverty as knowledge-optimized content loses resources, and subscriber surveillance infrastructure built to enable retention design. Each downstream constraint has its own extractiveness value and beneficiary/victim structure, but all are causally influenced by the retention incentive at the core.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(subscription_retention_imperative, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
