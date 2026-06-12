% ============================================================================
% CONSTRAINT STORY: wage_convergence_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wage_convergence_sustainability, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wage_convergence_sustainability
 *   human_readable: Wage Convergence Sustainability in China's Platform Economy
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   China's platform economy expansion from 2019-2025 has driven blue-collar
 *   wage growth (delivery drivers, warehouse workers, service providers) to
 *   outpace white-collar wage growth, narrowing the wage gap from
 *   approximately CNY2,250 in 2025 to a projected CNY2,020 in 2027. This
 *   convergence is driven by three structural forces: (1) demographic
 *   transition reducing young labor supply, (2) platform companies solving
 *   the matching problem between fragmented demand and mobile labor, and (3)
 *   policy intervention expanding social insurance and minimum wage coverage.
 *   The constraint's sustainability depends on platform profitability margins
 *   and continued policy support. The rising theater_ratio (0.25 → 0.48)
 *   reflects increasing performative policy rhetoric about 'common
 *   prosperity' and 'platform worker protection' that does not translate to
 *   structural enforcement. The rising extractiveness (0.15 → 0.32) reflects
 *   growing fragility: as platform margins compress and demographic pressure
 *   eases, the coordination mechanism may collapse or require escalating
 *   policy intervention to sustain.
 *
 * KEY AGENTS:
 *   - Blue-Collar Platform Workers: Primary beneficiary (powerless/mobile) — experience genuine wage gains through platform matching mechanisms; mobile exit options across platforms and sectors
 *   - White-Collar Workers: Mixed position (moderate/constrained) — benefit from overall economic coordination but experience relative wage stagnation; constrained by credential requirements and sector-specific skills
 *   - Platform Companies: Beneficiary (institutional/arbitrage) — solve two-sided market problem and benefit from labor supply expansion; arbitrage-level exit through business model adjustment or market exit
 *   - Policy Intervention Framework: Scaffold actor (institutional/constrained) — government agencies managing demographic transition through temporary support measures with implicit sunset logic
 *   - Labor Advocacy Organizations: Mixed position (organized/constrained) — validate worker claims through convergence but reveal structural dependency on platform business models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wage_convergence_sustainability, 0.28).
domain_priors:suppression_score(wage_convergence_sustainability, 0.35).
domain_priors:theater_ratio(wage_convergence_sustainability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wage_convergence_sustainability, extractiveness, 0.28).
narrative_ontology:constraint_metric(wage_convergence_sustainability, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(wage_convergence_sustainability, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wage_convergence_sustainability, rope).
narrative_ontology:human_readable(wage_convergence_sustainability, "Wage Convergence Sustainability in China's Platform Economy").
narrative_ontology:topic_domain(wage_convergence_sustainability, "labor_economics/platform_economy/social_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wage_convergence_sustainability, blue_collar_platform_workers).
narrative_ontology:constraint_beneficiary(wage_convergence_sustainability, platform_companies).
narrative_ontology:constraint_beneficiary(wage_convergence_sustainability, demographic_transition_policy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wage_convergence_sustainability, white_collar_workers).
narrative_ontology:constraint_vindicates(wage_convergence_sustainability, market_clearing_wage_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Delivery drivers, warehouse workers, and service providers experiencing wage growth through platform expansion. Can switch between platforms (Meituan, Ele.me, JD Logistics) or return to traditional employment. Wage gains are real but depend on continued platform demand and competitive labor markets.
narrative_ontology:constraint_stakeholder(wage_convergence_sustainability, blue_collar_platform_workers, beneficiary,
    powerless, biographical, mobile, national).

% Office workers, professionals, and technical staff experiencing wage stagnation as blue-collar wages rise. Constrained by credential requirements, sector-specific skills, and geographic concentration in tier-1 cities. Benefit from overall economic coordination but bear cost through relative position loss.
narrative_ontology:constraint_stakeholder(wage_convergence_sustainability, white_collar_workers, payer,
    moderate, biographical, constrained, national).

% Meituan, Ele.me, JD Logistics, and other platform operators solving two-sided market matching problem. Higher blue-collar wages attract labor supply enabling platform expansion. Can adjust business models, automate operations, or exit markets if margins compress. Benefit from coordination function.
narrative_ontology:constraint_stakeholder(wage_convergence_sustainability, platform_companies, beneficiary,
    institutional, immediate, arbitrage, national).

% Government agencies managing demographic transition through social insurance expansion, minimum wage increases, and platform regulation. See wage convergence as temporary coordination requiring active support until demographic pressure eases and skill supply adjusts. Cannot abandon demographic policy goals but view intervention as time-limited.
narrative_ontology:constraint_stakeholder(wage_convergence_sustainability, policy_intervention_framework, agenda_setter,
    institutional, generational, constrained, national).

% Worker advocacy groups and labor NGOs documenting platform working conditions and wage trends. Organized capacity to coordinate across workers but constrained by regulatory environment. See wage convergence as validating worker claims but revealing structural dependency on platform profitability rather than worker bargaining power.
narrative_ontology:constraint_stakeholder(wage_convergence_sustainability, labor_advocacy_organizations, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms solve the matching problem between fragmented consumer demand and mobile labor supply across skill levels, enabling wage growth for blue-collar workers through market mechanisms while demographic pressure reduces young labor supply.
% TRANSFER_FUNCTION: Wage income flows from platform companies and consumers to blue-collar workers (delivery drivers, warehouse workers, service providers). Relative wage position transfers from white-collar workers to blue-collar workers as the wage gap narrows. Policy resources flow to social insurance expansion and minimum wage enforcement.
% ABSENT_VOICES: Informal sector workers outside platform coverage, rural migrants without urban residency permits, and workers in regions with limited platform penetration would object that convergence benefits only platform-accessible workers. They are excluded from the coordination mechanism by geographic, regulatory, and infrastructure barriers.
% DISAPPEARANCE_RATIONALE: If platform matching mechanisms disappeared, blue-collar workers would lose wage gains from competitive platform labor markets and return to fragmented traditional employment with lower bargaining power. White-collar wage stagnation would ease as skill premium restored. Platform companies would lose two-sided market coordination benefits. Policy framework would need alternative mechanisms to manage demographic transition.
% FOUNDING_PROBLEM: China's demographic transition (aging population, shrinking young labor force) created structural labor supply pressure while traditional employment mechanisms failed to efficiently match fragmented demand with mobile labor across skill levels, particularly for blue-collar work.
% FOUNDING_PROBLEM_CORROBORATION: Demographic data from National Bureau of Statistics confirms continued aging and labor force contraction. Platform company employment data and wage surveys from Ministry of Human Resources and Social Security document ongoing matching function. Labor economists and policy researchers outside platform companies confirm the structural labor pressure persists.
narrative_ontology:disappearance_verdict(wage_convergence_sustainability, world_rearranges).
narrative_ontology:founding_problem_status(wage_convergence_sustainability, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLUE-COLLAR PLATFORM WORKERS (ROPE) — Mobile exit options (can switch platforms or return to traditional employment) and genuine wage gains from platform expansion. Experience the convergence as coordination: platforms solve the matching problem between labor supply and demand, enabling wage growth through market mechanisms. Net beneficiary of the coordination function.
constraint_indexing:constraint_classification(wage_convergence_sustainability, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: WHITE-COLLAR WORKERS (TANGLED ROPE) — Constrained by credential requirements and sector-specific skills. Benefit from overall economic coordination but experience relative wage stagnation as blue-collar wages rise. Mixed position: the same labor market that enables their employment also compresses their wage premium. Moderate extraction through relative position loss.
constraint_indexing:constraint_classification(wage_convergence_sustainability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM COMPANIES (ROPE) — Arbitrage-level exit (can adjust business models, automate, or exit markets). Experience wage convergence as coordination mechanism: higher blue-collar wages attract labor supply, enabling platform expansion. Benefit from solving the two-sided market problem. Low effective extraction — the constraint enables rather than restricts their operation.
constraint_indexing:constraint_classification(wage_convergence_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY INTERVENTION FRAMEWORK (SCAFFOLD) — Government agencies managing demographic transition see wage convergence as temporary coordination requiring active support. Social insurance expansion, platform regulation, and minimum wage policies are transitional measures with implicit sunset: once demographic pressure eases and skill supply adjusts, market mechanisms should sustain convergence without intervention. Constrained exit (cannot abandon demographic policy goals) but sees the intervention as time-limited.
constraint_indexing:constraint_classification(wage_convergence_sustainability, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents advocating for worker protections see both coordination (wage growth benefits workers) and extraction (sustainability depends on platform profitability, not worker power). Constrained exit (cannot abandon advocacy mission). Experience mixed extraction: the convergence validates their claims but its fragility reveals structural dependency on platform business models.
constraint_indexing:constraint_classification(wage_convergence_sustainability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, wage convergence driven by demographic pressure and platform expansion represents genuine market coordination solving a collective action problem: matching labor supply to demand across skill levels. The sustainability question is an empirical uncertainty (omega variable), not evidence of extraction. Low extractiveness reflects that the mechanism operates through voluntary exchange with mobile exit options for primary participants.
constraint_indexing:constraint_classification(wage_convergence_sustainability, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wage_convergence_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wage_convergence_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wage_convergence_sustainability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(wage_convergence_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate and rising. The convergence mechanism operates primarily through voluntary market coordination (platform matching, demographic supply pressure) rather than coercive extraction. However, extractiveness is rising (0.15 → 0.32 over interval) because sustainability increasingly depends on factors outside worker control: platform profitability, policy continuity, demographic trajectory. White-collar workers experience modest extraction through relative position loss, but this is market adjustment rather than targeted extraction. The value reflects genuine coordination with growing fragility rather than pure extraction. Suppression (0.35): Low-moderate and rising. Blue-collar workers have mobile exit options (can switch platforms or return to traditional employment), and platforms face competitive labor markets. However, suppression is rising (0.20 → 0.35) as platform market concentration increases and policy intervention becomes necessary to sustain convergence — workers' bargaining position depends on continued platform expansion and policy support rather than intrinsic market power. White-collar workers face moderate suppression through credential requirements and sector lock-in. Theater ratio (0.42): Moderate and rising sharply. Policy rhetoric about 'common prosperity' and 'platform worker protection' has intensified (0.25 → 0.48 over interval) while structural enforcement remains limited. Much policy activity is performative signaling rather than binding constraint on platform business models. The rising theater reflects that sustainability depends on continued political commitment, not institutionalized mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same wage convergence phenomenon appears differently across structural positions. Blue-collar workers see rope (genuine market coordination benefiting them). White-collar workers see tangled_rope (coordination that compresses their relative position). Platform companies see rope (coordination enabling their business model). Policy actors see scaffold (temporary intervention with sunset logic). Labor advocates see tangled_rope (worker gains revealing structural fragility). The analytical observer sees rope (market coordination with empirical uncertainty about sustainability). The gap is not about disagreement over facts but about structural position: beneficiaries with mobile exit experience coordination; those with constrained exit and mixed positions experience extraction; those managing the transition see temporary intervention. No perspective sees snare (pure extraction) because the mechanism operates through voluntary exchange with genuine exit options for primary participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Blue-collar platform workers are primary beneficiaries with mobile exit options — the engine derives low d (beneficiary status + mobile exit) producing low or negative effective extraction. They experience the constraint as genuine coordination solving their employment matching problem. White-collar workers are in a mixed position: beneficiaries of overall economic coordination but experiencing relative wage compression. The engine derives moderate d (mixed beneficiary/victim status + constrained exit) producing moderate effective extraction — they see tangled_rope. Platform companies are institutional beneficiaries with arbitrage exit — the engine derives very low d producing negative effective extraction (subsidy). They experience the constraint as enabling their business model. Policy framework actors see scaffold: temporary coordination requiring active support with implicit sunset. Labor advocacy organizations see tangled_rope: genuine worker gains that reveal structural dependency on platform profitability. The analytical observer sees rope: market coordination solving a collective action problem, with sustainability as an empirical uncertainty rather than evidence of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine market coordination (rope) from its fragility (rising extractiveness and theater). The convergence is not a snare — blue-collar workers have mobile exit options and experience real wage gains through voluntary platform participation. It is not a mountain — the mechanism depends on contingent factors (platform profitability, policy support, demographic pressure) rather than natural law. It is rope with rising fragility: the coordination function is real (platforms solve the matching problem), but sustainability depends on factors outside the coordination mechanism itself (platform margins, policy continuity). The scaffold perspective captures the policy dimension: intervention is transitional, with implicit sunset once demographic adjustment completes. The tangled_rope perspectives (white-collar workers, labor advocates) capture the mixed experience of those who benefit from coordination but bear costs through relative position loss or revealed dependency. The rising theater_ratio and extractiveness measurements show the constraint's trajectory: genuine coordination becoming more fragile and more dependent on performative policy support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_profitability_threshold,
    'At what margin compression level do platform companies reduce blue-collar employment or automate, reversing wage convergence?',
    'Longitudinal tracking of platform company margins, employment levels, and automation investment; identification of margin thresholds triggering strategic shifts',
    'If threshold is high (>15% margin): convergence is robust to cost pressure, validating rope classification. If threshold is low (<8% margin): convergence is fragile, suggesting tangled_rope or scaffold with shorter sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_profitability_threshold, empirical, 'Platform margin threshold for employment strategy shift').

omega_variable(
    demographic_reversal_timeline,
    'When does China''s demographic transition reverse labor supply pressure, and does wage convergence persist after reversal?',
    'Demographic projections combined with wage trajectory analysis; comparison of wage convergence in regions with different demographic profiles',
    'If convergence persists post-reversal: genuine market coordination (rope). If convergence collapses: demographic pressure was masking extraction or the coordination was temporary (scaffold confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_reversal_timeline, empirical, 'Demographic pressure reversal and wage persistence').

omega_variable(
    skill_premium_restoration,
    'Is white-collar wage stagnation a temporary adjustment or permanent compression of skill premium?',
    'Cross-national comparison of skill premium trajectories in platform economies; analysis of credential inflation and skill supply elasticity',
    'If temporary: rope classification holds (coordination adjusting to supply shock). If permanent: white-collar workers are victims of structural extraction, upgrading to tangled_rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_premium_restoration, empirical, 'Permanence of skill premium compression').

omega_variable(
    policy_intervention_necessity,
    'Would wage convergence continue without policy intervention (minimum wage, social insurance expansion, platform regulation)?',
    'Natural experiment comparison across regions with different policy intensities; counterfactual modeling of wage trajectories under policy withdrawal',
    'If convergence is policy-dependent: scaffold classification confirmed, intervention is structural rather than catalytic. If convergence is market-driven: rope classification holds, policy is accelerating rather than creating the trend.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_intervention_necessity, empirical, 'Policy dependence of wage convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wage_convergence_sustainability, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wage_conv_theater_2019, wage_convergence_sustainability, theater_ratio, 0, 0.25).
narrative_ontology:measurement(wage_conv_theater_2021, wage_convergence_sustainability, theater_ratio, 2, 0.3).
narrative_ontology:measurement(wage_conv_theater_2023, wage_convergence_sustainability, theater_ratio, 4, 0.35).
narrative_ontology:measurement(wage_conv_theater_2025, wage_convergence_sustainability, theater_ratio, 6, 0.42).
narrative_ontology:measurement(wage_conv_theater_2027_projected, wage_convergence_sustainability, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(wage_conv_extract_2019, wage_convergence_sustainability, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wage_conv_extract_2021, wage_convergence_sustainability, base_extractiveness, 2, 0.18).
narrative_ontology:measurement(wage_conv_extract_2023, wage_convergence_sustainability, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(wage_conv_extract_2025, wage_convergence_sustainability, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(wage_conv_extract_2027_projected, wage_convergence_sustainability, base_extractiveness, 8, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(wage_conv_suppress_2019, wage_convergence_sustainability, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(wage_conv_suppress_2025, wage_convergence_sustainability, suppression_requirement, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wage_convergence_sustainability, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of platform_flexibility_precarity_tradeoff (tangled_rope: platform work enables wage growth but creates precarity) and demographic_skill_mismatch (mountain: aging population and skill supply rigidity create structural labor pressure). The wage convergence constraint has its own extractiveness reflecting sustainability fragility, distinct from the upstream constraints' extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
