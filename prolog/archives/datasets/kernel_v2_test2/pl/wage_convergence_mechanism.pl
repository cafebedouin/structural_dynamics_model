% ============================================================================
% CONSTRAINT STORY: wage_convergence_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wage_convergence_mechanism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: wage_convergence_mechanism
 *   human_readable: Platform-Mediated Wage Convergence in Chinese Labor Markets
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The platform-mediated wage convergence mechanism in Chinese labor markets
 *   represents a structural transformation where algorithmic intermediation
 *   of urban service demand drives blue-collar wage growth (10%+ CAGR for
 *   delivery riders, CNY 37.30/hour; CNY 10,128/month for maternity
 *   caregivers) faster than white-collar wage growth, narrowing the
 *   occupational wage gap from CNY 3,344 (2013) to CNY 2,250 (2025). This
 *   constraint operates at the intersection of labor market coordination,
 *   employment classification disputes, and social insurance externalization.
 *   The mechanism exhibits genuine coordination — it solves the collective
 *   action problem of matching fragmented urban service demand with
 *   rural-urban migrant labor supply — but the coordination is inseparable
 *   from extraction mechanisms: platforms externalize social insurance costs
 *   to workers and the state, erode traditional employment security norms,
 *   and capture intermediation rents through algorithmic management. The
 *   constraint is embedded in a contested commitment system: the employment
 *   boundary kernel (what counts as employment triggering employer
 *   obligations versus independent contracting) remains textually stable in
 *   Chinese labor law, but platform companies and state regulators interpret
 *   it to exclude 300M flexible workers from full employment protections,
 *   absorbing massive structural drift without surfacing the need for kernel
 *   revision. Three readings compete: formalist (platform workers are
 *   contractors by contract form), substantive (platform workers are
 *   employees by economic dependence), and hybrid security (platform workers
 *   need tailored third-category protections). The wage convergence
 *   mechanism's classification depends critically on which reading prevails
 *   and whether the blue-collar wage growth is sustainable or a temporary
 *   platform subsidy phase.
 *
 * KEY AGENTS:
 *   - Blue-Collar Platform Workers: Primary beneficiary (powerless/mobile) — 10%+ wage CAGR, flexible scheduling, but lack employment security and adequate social insurance
 *   - White-Collar Wage Earners: Victim (moderate/constrained) — relative wage position erodes as gap narrows; career-specific human capital and urban cost-of-living create exit constraints
 *   - Platform Companies: Primary beneficiary (institutional/arbitrage) — capture intermediation rents, externalize labor costs, exploit regulatory arbitrage on employment classification
 *   - State Social Insurance System: Victim (institutional/constrained) — coordinates employment for 300M workers but bears unfunded pension and healthcare liabilities as platforms avoid employer contributions
 *   - Urban Service Consumers: Beneficiary (moderate/mobile) — access to affordable on-demand services (food delivery, domestic care, transportation) enabled by platform wage arbitrage
 *   - Labor Policy Reform Coalition: Organized agents (organized/constrained) — labor scholars, worker advocacy groups, progressive regulators attempting to build hybrid security framework with sunset logic
 *   - Traditional Employment Security: Abstract victim (powerless/trapped) — the normative expectation of stable employment with benefits erodes as platform flexibility becomes normalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wage_convergence_mechanism, 0.28).
domain_priors:suppression_score(wage_convergence_mechanism, 0.42).
domain_priors:theater_ratio(wage_convergence_mechanism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wage_convergence_mechanism, extractiveness, 0.28).
narrative_ontology:constraint_metric(wage_convergence_mechanism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wage_convergence_mechanism, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wage_convergence_mechanism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wage_convergence_mechanism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wage_convergence_mechanism, rope).
narrative_ontology:human_readable(wage_convergence_mechanism, "Platform-Mediated Wage Convergence in Chinese Labor Markets").
narrative_ontology:topic_domain(wage_convergence_mechanism, "labor_economics/platform_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wage_convergence_mechanism, 'bba4cae5-a699-4ee8-b8ac-035dc2a07bb3').
narrative_ontology:cs_kernel_codification('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', formalized).
narrative_ontology:cs_authority_grounding('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', extraction).
narrative_ontology:cs_interpretation_layer_present('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3').
narrative_ontology:cs_reading_relation('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', wage_convergence_mechanism__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', wage_convergence_mechanism__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', foundational, contract_form_determines_employment_status).
narrative_ontology:cs_axiom_status(contract_form_determines_employment_status, holdable).
narrative_ontology:cs_axiom_grounding('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', contract_form_determines_employment_status, conventional).
narrative_ontology:cs_axiom('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', secondary, algorithmic_intermediation_negates_employer_supervision).
narrative_ontology:cs_axiom_status(algorithmic_intermediation_negates_employer_supervision, holdable).
narrative_ontology:cs_axiom_grounding('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', algorithmic_intermediation_negates_employer_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', labor_contract_law_2008_employment_definition).
narrative_ontology:cs_drift_state('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', platform_economy_era_2025, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bba4cae5-a699-4ee8-b8ac-035dc2a07bb3', '2025-06-10T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wage_convergence_mechanism, blue_collar_platform_workers).
narrative_ontology:constraint_beneficiary(wage_convergence_mechanism, platform_companies).
narrative_ontology:constraint_beneficiary(wage_convergence_mechanism, urban_service_consumers).
narrative_ontology:constraint_victim(wage_convergence_mechanism, white_collar_wage_earners).
narrative_ontology:constraint_victim(wage_convergence_mechanism, traditional_employment_security).
narrative_ontology:constraint_victim(wage_convergence_mechanism, state_social_insurance_system).
narrative_ontology:constraint_vindicates(wage_convergence_mechanism, labor_market_flexibility_doctrine).
narrative_ontology:constraint_vindicates(wage_convergence_mechanism, platform_intermediation_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rural-urban migrants and urban low-skill workers who access platform-mediated gig work (food delivery, ride-hailing, domestic services). Experience 10%+ annual wage growth (CNY 37.30/hour for delivery riders, CNY 10,128/month for maternity caregivers) and flexible scheduling, but lack employment security, adequate pension contributions, and career development pathways. Can switch between platforms, return to traditional employment, or exit the labor force, giving them meaningful mobility despite powerless structural position.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, blue_collar_platform_workers, beneficiary,
    powerless, biographical, mobile, national).

% Urban professionals and office workers whose relative wage position erodes as the blue-collar/white-collar wage gap narrows from CNY 3,344 (2013) to CNY 2,250 (2025). Constrained by career-specific human capital (professional credentials, industry experience), urban cost-of-living (housing, education), and professional identity. Can resist through credential inflation and political mobilization but cannot easily exit their occupational track.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, white_collar_wage_earners, payer,
    moderate, biographical, constrained, national).

% Technology companies (Meituan, Didi, Ele.me, 58.com) that algorithmically intermediate urban service demand and labor supply. Set the terms of platform work through algorithmic management, pricing algorithms, and contract classification. Capture intermediation rents while externalizing social insurance costs to workers and the state. Can restructure business models, relocate operations, or pivot to other markets if regulatory pressure intensifies.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, platform_companies, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(wage_convergence_mechanism, platform_companies, beneficiary).

% Government agencies administering social insurance (pension, medical, unemployment, injury) that coordinate employment for 300M flexible workers but bear unfunded liabilities as platforms avoid employer contributions. Medical coverage reaches 91.5% and injury coverage 86.2% of platform workers through pilot programs, but pension and unemployment coverage remain low. Cannot abandon social insurance mandate but lacks enforcement capacity to compel platform reclassification.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, state_social_insurance_system, payer,
    institutional, generational, constrained, national).

% Urban residents who access affordable on-demand services (food delivery, ride-hailing, domestic care, logistics) enabled by platform wage arbitrage. Benefit from service availability, price competition, and convenience without bearing the social insurance externalization costs directly. Can switch between platforms or revert to traditional service providers.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, urban_service_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Labor scholars, worker advocacy groups, and progressive regulators attempting to build hybrid security framework (occupational injury insurance pilots, platform-specific social insurance schemes, third-category employment status proposals). See the wage convergence as transitional — demographic aging and pension underfunding will force structural reform within 10-15 years. Constrained by platform political influence and state preference for labor market flexibility.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, labor_policy_reform_coalition, observer,
    organized, generational, constrained, national).

% The normative expectation of stable employment with benefits (pension, medical, unemployment insurance, job security, career development) that erodes as platform flexibility becomes normalized. Not a real-world actor but an abstract institutional good that cannot organize or advocate for itself. Excluded from the policy conversation as platforms and state frame flexibility as worker preference rather than as erosion of employment norms.
narrative_ontology:constraint_stakeholder(wage_convergence_mechanism, traditional_employment_security, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(wage_convergence_mechanism, traditional_employment_security).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The wage convergence mechanism solves the collective action problem of matching fragmented urban service demand (millions of consumers needing food delivery, ride-hailing, domestic care, logistics at unpredictable times) with rural-urban migrant labor supply (workers seeking flexible income without long-term employment commitments). Traditional employment could not coordinate this matching at scale — the transaction costs of bilateral negotiation, the information asymmetry between consumers and workers, and the temporal mismatch between demand spikes and fixed employment schedules created persistent market failure. Platform algorithmic intermediation aggregates demand, routes tasks, sets prices, and manages quality at scale, enabling the labor market to clear.
% TRANSFER_FUNCTION: The mechanism moves money from urban service consumers to blue-collar platform workers (wage payments), from blue-collar workers to platform companies (commission fees, typically 15-25% of transaction value), and from the state social insurance system to platform workers (medical and injury coverage through pilot programs). It also moves relative economic position from white-collar wage earners to blue-collar platform workers (wage gap narrowing). The transfer is mediated by algorithmic pricing and task allocation rather than by bilateral negotiation or collective bargaining.
% ABSENT_VOICES: White-collar workers whose relative wage position erodes are present in labor market discourse but lack organized representation on platform labor policy — professional associations focus on credential protection rather than wage convergence resistance. Traditional employment security advocates (labor unions, employment law scholars emphasizing job stability) are marginalized in policy discussions that frame flexibility as worker preference. Future generations of platform workers who will retire without adequate pensions are entirely absent — the policy conversation focuses on immediate income gains and injury protection, not on long-term retirement security. Rural workers who lack digital literacy or urban hukou and cannot access platform work are excluded from the 'flexible employment' narrative.
% DISAPPEARANCE_RATIONALE: If the wage convergence mechanism disappeared overnight — platforms shut down, algorithmic intermediation ceased — the urban service economy would rearrange substantially. Blue-collar workers would lose flexible income opportunities and revert to lower-wage traditional employment or informal work (the 10%+ CAGR would reverse). Urban consumers would lose access to affordable on-demand services and revert to higher-cost traditional providers or self-provision. White-collar workers would see their relative wage position stabilize or improve. The state social insurance system would face lower immediate coverage obligations but also lose the pilot programs' experimental data. Platform companies would lose intermediation rents. The rearrangement would be substantial and immediate — this is not a natural fact that would persist regardless of institutional arrangements. The constraint is constructed, not discovered.
% FOUNDING_PROBLEM: The wage convergence mechanism was built to solve the urban service demand fragmentation problem that emerged with China's rapid urbanization (2000-2020): millions of urban consumers needed flexible access to services (food delivery, transportation, domestic care) at unpredictable times, but traditional employment structures (fixed schedules, bilateral negotiation, high search costs) could not coordinate supply and demand efficiently. Simultaneously, rural-urban migrants needed flexible income opportunities that did not require long-term employment commitments or urban hukou. The founding problem was genuine market failure — fragmented demand and supply could not find each other at scale through traditional mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, corroborated by: (1) Urban service consumers continue to use platforms at high rates (food delivery penetration >60% in tier-1 cities, ride-hailing >50%), indicating that the coordination problem persists and platforms solve it. (2) Blue-collar workers continue to enter platform work despite awareness of social insurance gaps, indicating that the flexible income opportunity remains valuable relative to alternatives. (3) Labor economists and urban planning scholars document persistent demand-supply mismatch in urban service sectors when platforms are restricted (e.g., ride-hailing caps in some cities lead to service shortages and price spikes). (4) Platform companies' continued profitability and market expansion indicate that the intermediation function remains valuable, not merely a subsidy-driven market capture phase. The corroboration comes from multiple seats: consumers (revealed preference), workers (labor supply decisions), scholars (empirical analysis), and market outcomes (platform sustainability). This is not a beneficiary-only narrative.
narrative_ontology:disappearance_verdict(wage_convergence_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(wage_convergence_mechanism, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLUE-COLLAR PLATFORM WORKER (ROPE) — Experiences the wage convergence as genuine coordination solving income precarity. Mobile exit (can switch platforms, return to traditional employment, or exit labor force) and net beneficiary status (10%+ wage CAGR, CNY 37.30/hour for delivery riders vs previous options) produce low effective extraction. The constraint coordinates demand aggregation and flexible scheduling that traditional employment could not provide.
constraint_indexing:constraint_classification(wage_convergence_mechanism, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: WHITE-COLLAR WAGE EARNER (TANGLED ROPE) — Experiences both coordination (labor market operates, employment exists) and extraction (relative wage position erodes as blue-collar wages rise faster). Constrained exit (career-specific human capital, urban cost-of-living lock-in, professional identity) and victim status (wage gap narrowing from CNY 3,344 to CNY 2,250) produce moderate effective extraction. The same platform mechanism that coordinates blue-collar opportunity extracts from white-collar relative position.
constraint_indexing:constraint_classification(wage_convergence_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM COMPANY (ROPE) — Primary beneficiary experiencing the constraint as pure coordination. Arbitrage exit (can restructure business model, relocate operations, pivot to other markets) and beneficiary status (labor cost externalization, regulatory arbitrage on employment classification, demand aggregation rents) produce negative effective extraction. The wage convergence mechanism is the platform's core value proposition — it coordinates fragmented demand and supply while capturing intermediation rents.
constraint_indexing:constraint_classification(wage_convergence_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SOCIAL INSURANCE SYSTEM (TANGLED ROPE) — Experiences mixed coordination and extraction. The platform economy coordinates employment for 300M flexible workers (genuine coordination function), but externalizes social insurance costs to the state and workers. Constrained exit (cannot abandon social insurance mandate, but lacks enforcement capacity over platforms) and victim status (bears unfunded liabilities as platform workers age without adequate retirement savings) produce moderate extraction. The hybrid security reading attempts to resolve this through occupational injury insurance pilots, but the extraction persists.
constraint_indexing:constraint_classification(wage_convergence_mechanism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR POLICY REFORM COALITION (SCAFFOLD) — Organized agents (labor scholars, worker advocacy groups, progressive regulators) see the wage convergence as a transitional coordination mechanism with a sunset. The current platform-mediated flexibility solves immediate income needs but cannot persist as the sole labor market structure — demographic aging, pension underfunding, and career development deficits will force either platform reclassification (substantive employment reading) or comprehensive third-category protections (hybrid security reading). The coalition sees a 10-15 year window before the contradictions force structural reform.
constraint_indexing:constraint_classification(wage_convergence_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the wage convergence mechanism exhibits both genuine coordination (solves the collective action problem of matching fragmented urban service demand with rural-urban migrant labor supply) and asymmetric extraction (externalizes long-term social insurance costs, erodes employment security norms, concentrates platform rents). The mechanism requires active enforcement through algorithmic management and regulatory tolerance of employment misclassification. The analytical classification is tangled_rope, not rope, because the coordination function is inseparable from the extraction mechanism — the same algorithmic intermediation that enables flexible income also prevents collective bargaining and shifts risk to workers.
constraint_indexing:constraint_classification(wage_convergence_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wage_convergence_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wage_convergence_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wage_convergence_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(wage_convergence_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The mechanism produces genuine wage gains for blue-collar workers (10%+ CAGR) and coordinates previously fragmented labor supply, but extraction occurs through three channels: (1) social insurance cost externalization to workers and state (medical coverage 91.5%, injury 86.2%, but pension and unemployment coverage remain low), (2) erosion of white-collar relative wage position (gap narrowing from CNY 3,344 to CNY 2,250), and (3) platform capture of intermediation rents through algorithmic management. The extractiveness has increased over the interval (0.15 to 0.28) as platform market power consolidated and the social insurance gap became more visible with worker aging. The value is moderate-low rather than high because the blue-collar wage gains are real and substantial — this is not pure extraction disguised as coordination. Suppression (0.42): Moderate. Barriers to alternative arrangements include: algorithmic management that prevents collective bargaining, regulatory tolerance of employment misclassification that blocks worker reclassification claims, platform market concentration that limits worker bargaining power, and hukou system constraints that limit rural workers' urban employment options. Suppression has increased (0.30 to 0.42) as platforms consolidated market share and algorithmic management intensified. But suppression is not total — workers retain mobility across platforms, can return to traditional employment, and some regulatory experiments (injury insurance pilots) are reducing barriers. Theater ratio (0.35): Moderate-low. Some performative elements exist: platforms claim to provide 'entrepreneurial opportunities' and 'flexible work' while deploying algorithmic management that resembles traditional employment supervision; state regulators claim to protect platform workers through pilot programs while tolerating widespread employment misclassification; the 'independent contractor' classification is partly theatrical given the economic dependence and algorithmic control. Theater has increased (0.20 to 0.35) as the gap between platform rhetoric and worker reality widened. But the theater is not dominant — the wage gains are real, the coordination function is genuine, and the regulatory experiments represent real policy search rather than pure performance. Accessibility collapse (0.40): Moderate. Once the platform economy's wage arbitrage and flexibility benefits are understood, some alternatives do collapse — traditional employment's rigid scheduling and lower blue-collar wages become less attractive, and the urban service economy's previous informal arrangements (unmediated domestic workers, street-hail taxis) largely disappear. But alternatives persist: workers can still choose traditional employment, self-employment, or exit the labor force; white-collar workers can resist convergence through credentialing and political mobilization; the state can reclassify platform workers or build hybrid protections. The moderate value reflects that the platform model is powerful but not inevitable. Resistance (0.55): Moderate-high. The mechanism meets substantial resistance: white-collar workers resist wage convergence through credential inflation and professional association lobbying; labor scholars and worker advocates push for substantive employment reading or hybrid protections; some local courts rule in favor of platform worker employment status; traditional taxi and domestic service sectors resist platform displacement; the state faces fiscal pressure from pension underfunding. The resistance is real and organized, not merely individual complaint, which distinguishes this from a low-resistance natural law.
 *
 * PERSPECTIVAL GAP:
 *   The wage convergence mechanism demonstrates how the same structural phenomenon produces radically different classifications depending on the observer's position in the extraction flow. Blue-collar platform workers experience rope — genuine coordination solving their income precarity, with mobile exit and net benefits. White-collar wage earners experience tangled_rope — the same mechanism that coordinates blue-collar opportunity extracts from their relative position, with constrained exit due to career-specific human capital. Platform companies experience rope — pure coordination from their perspective, capturing intermediation rents while externalizing costs. The state social insurance system experiences tangled_rope — coordinates employment for 300M workers but bears unfunded liabilities. The labor policy reform coalition sees scaffold — a transitional mechanism with a sunset as demographic aging forces structural reform. The analytical observer sees tangled_rope — genuine coordination inseparable from extraction, requiring active enforcement through algorithmic management and regulatory tolerance. The perspectival gap is not a measurement error — it is the structure of the constraint itself. The mechanism IS coordinative from the blue-collar worker's biographical perspective and extractive from the white-collar worker's biographical perspective and transitional from the reform coalition's generational perspective. No single type captures the full structure; the presheaf over observation sites is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Blue-collar platform workers are beneficiaries with mobile exit — they can switch platforms, return to traditional employment, or exit the labor force, and they experience net wage gains. This produces low d (near beneficiary end) and low or negative effective extraction. White-collar wage earners are victims with constrained exit — career-specific human capital, urban cost-of-living, and professional identity create exit barriers, and they experience relative wage erosion. This produces moderate-high d and moderate effective extraction. Platform companies are beneficiaries with arbitrage exit — they can restructure, relocate, or pivot, and they capture intermediation rents. This produces very low d and negative effective extraction (they extract from others). The state social insurance system is a victim with constrained exit — it cannot abandon its mandate but lacks enforcement capacity over platforms, and it bears unfunded liabilities. This produces moderate-high d and moderate effective extraction. The directionality derivation captures the structural asymmetry: the same mechanism that benefits blue-collar workers and platforms extracts from white-collar workers and the state insurance system. No override is needed — the structural declarations produce the correct directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The wage convergence mechanism resolves the mandatrophy by demonstrating that rope and tangled_rope are both structurally accurate classifications from different observation sites. The mechanism is NOT pure coordination (rope from all perspectives) because white-collar workers and the state insurance system are genuine victims experiencing extraction. It is NOT pure extraction (snare from all perspectives) because blue-collar workers experience genuine wage gains and coordination benefits. The analytical classification is tangled_rope because the coordination function (matching fragmented demand and supply) is inseparable from the extraction mechanism (social insurance externalization, white-collar wage erosion, platform rent capture). The mechanism requires active enforcement through algorithmic management and regulatory tolerance of employment misclassification — it does not persist through voluntary coordination alone. The mandatrophy is resolved by recognizing that the constraint's type is observer-dependent: rope from the blue-collar worker's seat, tangled_rope from the white-collar worker's and state's seats, scaffold from the reform coalition's seat. The presheaf structure is the constraint's true form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_convergence_durability,
    'Is the blue-collar wage growth rate (10%+ CAGR) sustainable, or does it reflect a temporary platform subsidy phase that will end once market dominance is established?',
    'Longitudinal wage tracking across platform maturity stages; comparison of wage trajectories in markets where platforms achieved dominance (2015-2020) versus emerging markets (2020-2025); analysis of platform unit economics and subsidy burn rates.',
    'If sustainable: rope classification holds — genuine coordination with durable benefits. If temporary: reclassify toward snare — the wage convergence is bait in a market-capture strategy, and extraction will intensify post-dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_durability, empirical, 'Whether platform-mediated blue-collar wage growth is sustainable or temporary subsidy').

omega_variable(
    employment_boundary_resolution,
    'Will the employment boundary kernel be revised to include platform workers (substantive reading), remain stable excluding them (formalist reading), or institutionalize a hybrid third category (hybrid security reading)?',
    'Tracking of regulatory experiments (occupational injury insurance pilots, platform-specific social insurance schemes); Supreme People''s Court guidance on platform employment disputes; State Council policy evolution on ''new employment forms''; comparative analysis with EU Platform Work Directive and California AB5.',
    'Substantive reading: platforms become obligated employers, extractiveness increases as they resist reclassification. Formalist reading: current structure persists, extractiveness stable. Hybrid reading: partial protections institutionalize, moderate extractiveness as precarity is normalized with basic safety net.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employment_boundary_resolution, preference, 'Which reading of the employment boundary kernel will prevail').

omega_variable(
    demographic_aging_collision,
    'When will the demographic aging crisis (peak retirement burden 2035-2050) collide with the platform economy''s pension underfunding, and what structural adjustment will result?',
    'Actuarial modeling of pension system solvency under current platform worker contribution rates; projection of platform worker cohort size and age distribution; identification of fiscal breaking points where state must either mandate platform contributions or accept pension system collapse.',
    'Early collision (pre-2030): forces rapid reclassification or hybrid system, confirming scaffold perspective. Late collision (post-2040): allows current extraction to persist for another generation, weakening scaffold claim and strengthening tangled_rope as stable state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_aging_collision, empirical, 'Timing and outcome of demographic aging collision with platform pension underfunding').

omega_variable(
    white_collar_resistance_threshold,
    'At what wage gap threshold will white-collar workers organize politically to resist further convergence, and what mechanisms will they deploy?',
    'Monitoring of white-collar labor organizing (professional associations, union formation, political lobbying); tracking of policy proposals to restrict platform labor supply (hukou reform resistance, occupational licensing expansion, credential inflation); analysis of urban middle-class political mobilization around labor market anxiety.',
    'Low threshold (CNY 2,000 gap): white-collar resistance emerges soon, potentially blocking further convergence and revealing the mechanism as contested rather than coordinative. High threshold (CNY 1,000 gap): convergence continues with minimal resistance, supporting rope classification from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(white_collar_resistance_threshold, empirical, 'White-collar political resistance threshold to wage convergence').

omega_variable(
    cs_framing_underdetermination,
    'Is the employment boundary kernel best framed as the legal definition in Labor Contract Law (formalist framing) or as the broader legitimacy claim about what work arrangements deserve protection (substantive framing)?',
    'Analysis of which framing better predicts regulatory outcomes, worker mobilization patterns, and platform strategic responses; examination of whether disputes are resolved through textual interpretation (formalist) or through appeals to economic substance and worker vulnerability (substantive).',
    'Formalist framing: the kernel is the legal text, and drift is absorbed through interpretation that excludes platform workers. Substantive framing: the kernel is the legitimacy claim about protection-worthy work, and the legal text is merely one instantiation — drift surfaces as a gap between the claim and the text''s coverage. Different framings produce different cs_pattern classifications and different predictions about where the system will break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether employment boundary kernel is legal text or legitimacy claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wage_convergence_mechanism, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wage_conv_theater_2013, wage_convergence_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wage_conv_theater_2016, wage_convergence_mechanism, theater_ratio, 3, 0.25).
narrative_ontology:measurement(wage_conv_theater_2019, wage_convergence_mechanism, theater_ratio, 6, 0.28).
narrative_ontology:measurement(wage_conv_theater_2022, wage_convergence_mechanism, theater_ratio, 9, 0.32).
narrative_ontology:measurement(wage_conv_theater_2025, wage_convergence_mechanism, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(wage_conv_extract_2013, wage_convergence_mechanism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wage_conv_extract_2016, wage_convergence_mechanism, base_extractiveness, 3, 0.18).
narrative_ontology:measurement(wage_conv_extract_2019, wage_convergence_mechanism, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(wage_conv_extract_2022, wage_convergence_mechanism, base_extractiveness, 9, 0.25).
narrative_ontology:measurement(wage_conv_extract_2025, wage_convergence_mechanism, base_extractiveness, 12, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(wage_conv_suppress_2013, wage_convergence_mechanism, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(wage_conv_suppress_2019, wage_convergence_mechanism, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(wage_conv_suppress_2025, wage_convergence_mechanism, suppression_requirement, 12, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wage_convergence_mechanism, resource_allocation).
narrative_ontology:affects_constraint(wage_convergence_mechanism, urban_service_demand_fragmentation).
narrative_ontology:affects_constraint(wage_convergence_mechanism, rural_urban_migration_flow).
narrative_ontology:affects_constraint(wage_convergence_mechanism, social_insurance_coverage_gap).

% DUAL FORMULATION NOTE:
% The wage convergence mechanism is downstream of urban service demand fragmentation (platforms aggregate fragmented demand) and rural-urban migration flow (platforms channel migrant labor supply), and upstream of social insurance coverage gap (wage convergence without employment reclassification widens the pension underfunding). Each constraint has its own extractiveness reflecting its specific structural dynamics; the network edges capture causal dependencies without collapsing them into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
