% ============================================================================
% CONSTRAINT STORY: gig_platform_algorithmic_management
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_platform_algorithmic_management, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gig_platform_algorithmic_management
 *   human_readable: Algorithmic Management Tightening on Gig Platform
 *   domain: platform_economy/labor
 *
 * SUMMARY:
 *   Algorithmic management on gig platforms operates as a multi-level
 *   coercion system where individual drivers, driver organizations, and labor
 *   markets experience sharply differentiated tightening over five years. At
 *   the individual level, accessibility of alternatives collapses as the
 *   dominant platform consolidates market share (0.68 → 0.85), stakes inflate
 *   as algorithmic deactivation threatens household income (0.62 → 0.79), and
 *   suppression hardens through rating systems and task-assignment opacity
 *   (0.58 → 0.76). Simultaneously, individual resistance deteriorates (0.28 →
 *   0.18) as atomized drivers lose bargaining power. At the organizational
 *   level (driver coalitions, unions), accessibility collapse is slower (0.52
 *   → 0.71) because organized actors retain some bargaining channels, but
 *   suppression still hardens (0.44 → 0.62) as the platform weaponizes
 *   algorithmic ranking against activist drivers. Class-level (labor market)
 *   alternatives also collapse (0.45 → 0.62) as the gig platform becomes the
 *   primary wage source for a growing cohort. Structurally, the market's
 *   regulatory latitude narrows (accessibility 0.38 → 0.51) as regulatory
 *   bodies begin imposing constraints, but suppression remains comparatively
 *   modest (0.32 → 0.41) because the platform's enforcement mechanism is
 *   algorithmic rather than legally formalized. The coercion profile shows
 *   systematic divergence: individual level experiences the steepest
 *   tightening (accessibility +0.17, stakes +0.17, suppression +0.18);
 *   organizational level mounts growing resistance (+0.09) even as
 *   suppression hardens; structural level sees incoming regulation but
 *   maintains comparative freedom. This divergence instantiates the
 *   tangled_rope classification: the platform coordinates task matching
 *   (genuine collective problem solved) while simultaneously extracting rent
 *   through algorithmic control, and the extraction mechanism hardens at the
 *   individual level faster than alternatives-provision occurs at any level.
 *
 * KEY AGENTS:
 *   - Dependent Driver: Primary victim (powerless/trapped) — single-platform income dependency, regional market concentration, no viable exit
 *   - Multi-App Driver: Secondary victim (moderate/constrained) — higher skills and geographic mobility, but algorithmic stratification and account-management friction limit exit feasibility
 *   - Driver Coalition/Union: Organized resistance (organized/constrained) — growing bargaining power but algorithmic ranking system divides membership, deactivation threat constrains militant action
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — extracts rent through algorithmic control; consolidates market power; views tightening as efficiency optimization
 *   - Regulatory Authority: External constraint (organized/mobile) — imposing algorithmic transparency and classification mandates; scaffold perspective with sunset carrying regulatory crystallization
 *   - Gig-Economy Ideology: Institutional performance (institutional/arbitrage) — maintains theater of driver autonomy even as algorithmic control deepens; piton classification reflecting degradation of actual flexibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_platform_algorithmic_management, 0.68).
domain_priors:suppression_score(gig_platform_algorithmic_management, 0.72).
domain_priors:theater_ratio(gig_platform_algorithmic_management, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_platform_algorithmic_management, extractiveness, 0.68).
narrative_ontology:constraint_metric(gig_platform_algorithmic_management, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gig_platform_algorithmic_management, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gig_platform_algorithmic_management, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(gig_platform_algorithmic_management, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_platform_algorithmic_management, tangled_rope).
narrative_ontology:human_readable(gig_platform_algorithmic_management, "Algorithmic Management Tightening on Gig Platform").
narrative_ontology:topic_domain(gig_platform_algorithmic_management, "platform_economy/labor").

domain_priors:requires_active_enforcement(gig_platform_algorithmic_management).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_platform_algorithmic_management, platform_operator).
narrative_ontology:constraint_victim(gig_platform_algorithmic_management, driver_workforce).
narrative_ontology:constraint_victim(gig_platform_algorithmic_management, labor_market_flexibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gig_platform_algorithmic_management, skilled_multi_app_driver).
narrative_ontology:constraint_victim(gig_platform_algorithmic_management, dependent_driver).
narrative_ontology:constraint_victim(gig_platform_algorithmic_management, skilled_multi_app_driver).
narrative_ontology:constraint_victim(gig_platform_algorithmic_management, driver_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drives 50+ hours per week for primary household income; has no training for alternative employment; regional labor market has no equivalent wages outside gig work. Platform is sole income source. Deactivation would mean household inability to pay rent within 2-3 weeks. Experiences task assignment as arbitrary and income as unpredictable; rating score is the only metric controlling access to high-demand work.
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, dependent_driver, payer,
    powerless, biographical, trapped, regional).

% Drives 30-40 hours per week while maintaining alternative income or job prospects; has college education or transferable skills; maintains accounts on 2-3 platforms to reduce dependence on any single algorithmic system. Can leave gig work within 3-6 months of financial planning, but faces friction from account management and algorithmic penalties for low acceptance rates. Experiences both task-matching benefit (predictable supply) and suppression (selective task assignment to high-compliance drivers).
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, skilled_multi_app_driver, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gig_platform_algorithmic_management, skilled_multi_app_driver, beneficiary).

% Organized driver collective with growing membership (15-25% of platform workforce in major cities by year 5). Negotiates with platform for scheduling predictability, minimum earnings guarantees, and deactivation appeal processes. Mounts public campaigns for regulatory classification change (employee status). Membership includes both dependent and skilled drivers; platform response is selective deactivation of coalition organizers, rating-system suppression for known activists. Has leveraged power (threat of collective withdrawal) but remains constrained by internal division and platform's ability to divide members through ranking.
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, driver_coalition, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gig_platform_algorithmic_management, driver_coalition, agenda_setter).

% Operates multi-market ride-sharing platform with >1 million active drivers globally by year 5. Extracts rent through algorithmic task assignment, commission rates, and selective deactivation of low-compliance drivers. Controls algorithmic design, pricing, and rating system; faces regulatory pressure in select jurisdictions but maintains unilateral control over driver-facing terms. Can exit market segments, pivot to autonomous vehicles, or consolidate competitors. Views algorithmic tightening as efficiency optimization and rational response to labor-market pressure.
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, platform_operator, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gig_platform_algorithmic_management, platform_operator, beneficiary).

% Municipal and state labor regulators tasked with implementing worker protections and labor standards. By year 5, has passed algorithmic transparency mandates, minimum-earnings requirements, and begun classification review processes in several jurisdictions. Can impose fines, restrict platform licensing, or mandate operational changes. Faces significant lobbying from platform and industry groups; effectiveness varies by jurisdiction (California, EU have stronger mandates; other regions have weaker enforcement). Has leverage through regulatory authority but faces capture risk and political opposition.
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, regulatory_authority, agenda_setter,
    organized, generational, mobile, national).

% The narrative of 'flexibility' and 'entrepreneurship' that frames gig-platform work as liberation from traditional employment. Persists despite empirical evidence of declining autonomy and rising algorithmic control. Serves as cover story legitimating the platform's unilateral authority and suppressing regulatory response. Maintained through media narratives, platform marketing, and ideological commitment from platform leadership and sympathetic policy makers. The narrative is not an agent but a non-agent institutional force that affects how the constraint is experienced and regulated.
narrative_ontology:constraint_stakeholder(gig_platform_algorithmic_management, gig_economy_narrative, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(gig_platform_algorithmic_management, gig_economy_narrative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gig_platform_algorithmic_management, platform_operator).
narrative_ontology:fixing_cost_class(gig_platform_algorithmic_management, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Task-to-driver matching: reducing search friction between demand for rides and available driver capacity. Pre-algorithmic matching required street hail or phone dispatch; algorithmic matching dramatically reduces wait times and increases utilization of driver capacity. This is a genuine coordination problem solved.
% TRANSFER_FUNCTION: The platform extracts: (1) Commission on every ride (15-30% of fare), (2) Information rents from driver behavior data used to optimize pricing and suppress alternatives, (3) Risk transfer of income volatility to drivers while platform maintains steady commission. The transfer flows from drivers to platform through algorithmic control of task assignment, pricing, and deactivation threat.
% ABSENT_VOICES: Prospective entrants (workers considering gig work who decide against it due to reputation of low pay and algorithmic control); workers who have exited and are unavailable for organizing or research; unemployed workers who could use gig work but face algorithmic barriers to entry; competitors to the dominant platform who have been displaced or prevented from entering; regulatory voices from jurisdictions where platform operates without consent (global south, unregulated markets). These groups would object if present but are structurally excluded from negotiation.
% DISAPPEARANCE_RATIONALE: If algorithmic management disappeared overnight, labor market would require major reorganization: dependent drivers would face immediate income crisis (housing insecurity, debt default) within weeks; cities would lose primary on-demand transportation supply within days (drivers would require new dispatch infrastructure or return to traditional taxi model); platform operator would lose primary profit mechanism (algorithmic control and commission extraction) and would be forced into alternative business models (autonomous vehicles, employee classification with wage/benefit obligations). The constraint structures household income, transportation systems, and platform valuation; its disappearance would rearrange all three.
% FOUNDING_PROBLEM: Transportation matching inefficiency: traditional taxi models required street hail or phone dispatch, leading to dead time for drivers (searching for passengers), long wait times for passengers (searching for taxis), and inefficient vehicle utilization. Algorithmic matching solved this by providing real-time supply-demand clearing.
% FOUNDING_PROBLEM_CORROBORATION: Pre-platform transportation economics documents show high deadweight loss from matching friction. Testimonials from drivers who switched from street hail to algorithmic platforms report higher utilization and reduced search time. However, by year 5 this corroboration is weakening: drivers report that algorithmic 'efficiency' now manifests as reduced per-ride income despite higher utilization; the efficiency gains are captured by the platform, not shared with drivers. External labor researchers and transportation economists corroborate that matching efficiency was genuinely improved but is now decoupled from driver welfare. The founding problem remains live (matching is still valuable) but the solution has been captured.
narrative_ontology:disappearance_verdict(gig_platform_algorithmic_management, world_rearranges).
narrative_ontology:founding_problem_status(gig_platform_algorithmic_management, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT DRIVER (SNARE) — Trapped by financial dependency and platform ubiquity; no structurally viable alternative labor market exists in the region; algorithm controls task assignment and income flow; exit costs are absolute (income replacement, retraining, geographic relocation). Experiences maximum extraction and suppression as the platform consolidates — alternatives collapse from 'difficult' to 'impossible'.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SKILLED MULTI-APP DRIVER (TANGLED ROPE) — Higher education, multiple app credentials, some geographic mobility; can exit to a second platform with friction but not impossibility. Experiences both coordination (task matching, payment certainty vs. street-hail) and extraction (algorithmic rating system determines income). High suppression from rating-based deactivation threat, but higher exit capacity than dependent drivers. Moderate experience of extraction asymmetry.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Views algorithmic management as pure coordination: matching supply to demand, optimizing utilization, reducing deadweight loss from mismatch. Beneficiary position with arbitrage options (can shift to autonomous vehicles, adjust geographic coverage, pivot to adjacent markets). Experiences minimal suppression — enforcement is codified, automated, and aligns with platform profitability. Net positive extraction flow.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DRIVER COALITION (TANGLED ROPE) — Organized collective action with growing but uneven power; some bargaining leverage from threat of withdrawal or public pressure, but platform's algorithmic ranking system divides the workforce (high-rated drivers less likely to unionize). Experiences both coordination function (the union negotiates scheduling predictability, fare guarantees) and extraction (platform retains unilateral algorithmic control despite negotiation). Moderate suppression from algorithmic ranking and threat of deactivation for 'troublemakers'.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (SCAFFOLD) — Views algorithmic management as a temporary coordination problem pending formal regulation. Municipal and state actors are legislating requirements for algorithmic transparency, minimum earnings guarantees, and classification status. The scaffold carries a sunset: as regulatory regimes crystallize (driver classification as employee vs. contractor, algorithmic disclosure mandates), the platform's unilateral control loses authority. Organized agent with mobile exit options (can impose fines, licensing restrictions, geographic exclusion). Moderate suppression from industry lobbying and legal uncertainty.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: GIG-ECONOMY IDEOLOGY (PITON) — The narrative of 'flexibility' and 'entrepreneurship' persists even as the platform consolidates control and drivers' actual autonomy declines. The theater of driver independence is maintained (drivers 'accept' or 'decline' tasks, set their own hours) while the algorithmic system makes meaningful rejection costly or impossible (low ratings reduce task flow, income volatility increases, deactivation hangs as permanent threat). The piton classification reflects high theater_ratio: the performative autonomy persists through institutional inertia even as the coordination function it claimed to enable is replaced by unilateral algorithmic extraction.
constraint_indexing:constraint_classification(gig_platform_algorithmic_management, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_platform_algorithmic_management_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_platform_algorithmic_management, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_platform_algorithmic_management, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_platform_algorithmic_management, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_platform_algorithmic_management, TR),
    TR >= 0.70.

:- end_tests(gig_platform_algorithmic_management_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The platform captures the wage differential created by algorithmic task assignment, rating stratification, and income volatility management. Base measurement of 0.38 at t0 reflects the constraint's early form when alternatives were plentiful and driver atomization was incomplete. Final measurement of 0.68 reflects consolidation: the dominant platform has captured regional market share exceeding 70%, drivers' multi-app strategies are becoming prohibitively costly due to algorithmic penalties, and income volatility is stabilized (at lower mean) only for high-rated drivers whose acceptance rates exceed 95%. The extraction mechanism is not overt rent collection but rather: (1) Task assignment pricing that undercuts pre-algorithmic wages due to labor oversupply, (2) Income volatility shifting financial risk to drivers while platform maintains steady commission flow, (3) Algorithmic rating system that extracts information (driver behavior, willingness to accept low-priced tasks) used to further optimize pricing. Suppression (0.72): High and rising. The mechanism is not physical coercion but algorithmic: (1) Deactivation threat for low ratings (< 4.6 stars), account suspension for 'problematic' behavior (unionization, accepting rival-platform tasks, contacting regulatory authorities), (2) Task assignment opacity making it impossible to verify fair-matching claims or challenge algorithmic decisions, (3) Rating inflation and soft penalties (lower task frequency) for drivers who decline low-priced assignments, reducing income without formal termination. Suppression_requirement measurement shows steady hardening: 0.54 → 0.72 over the interval. Theater ratio (0.58): Moderate and stable. The platform maintains performative autonomy — drivers 'choose' their hours, 'accept' or 'decline' tasks, set their vehicle preferences — but the algorithmic system constrains these choices: declined tasks reduce ratings and future task frequency, working 'unusual hours' incurs algorithmic penalties, vehicle preferences are ignored if supply is tight. The theater is substantial but less than pure piton (theater_ratio 0.72 in verification bottleneck); the coordinate problem is real (matching supply to demand is genuinely difficult), so the performance is not entirely fake. Accessibility collapse (0.81): Very high. Individual drivers face collapsing alternatives: competing platforms have reduced to 1-2 viable options in most markets, streetwork and traditional taxi medallions are economically non-viable due to capital requirements, full-time employment in equivalent wage jobs is scarce in lower-income regions, retraining into higher-income alternatives requires time and capital the gig-dependent cannot accumulate. Multi-app strategies are theoretically viable but practically costly (account switching friction, algorithmic penalties, learning curves across interface changes). Resistance (0.42): Moderate and declining. Individual drivers mount little resistance (0.18 by t5) due to atomization and fear of deactivation. Driver coalitions and unions mount growing resistance (0.61 by t5) through negotiation, legal challenges, and public organizing, but this resistance remains constrained by the platform's ability to selectively deactivate activist drivers and by regulatory capture risk (many jurisdictions lack labor standards covering gig workers).
 *
 * PERSPECTIVAL GAP:
 *   The dependent driver experiences snare (maximum extraction, immobilized by collapsed alternatives, no organized resistance available). The skilled multi-app driver experiences tangled_rope (partial extraction mitigated by skills and mobility, organized resistance available through multi-app strategies, but substantial algorithmic suppression from ranking system). The platform operator experiences rope (coordination of supply/demand, beneficiary position with low experienced suppression, arbitrage options into autonomous technology or geographic expansion). The driver coalition experiences tangled_rope (coordinating scheduling predictability and fare guarantees while being suppressed by selective deactivation of organizers). The regulatory authority experiences scaffold (imposing algorithmic transparency and worker protections, but carrying risk of capture and false sunset). The gig-economy ideology experiences piton (the narrative of autonomy persists as performative theater despite algorithmic control deepening). The divergence is not random: beneficiaries experience rope, victims experience snare or tangled_rope, organized agents experience scaffold or tangled_rope, and institutional narratives experience piton. The perspectival gap is fundamentally the gap between experienced exit capacity and structural exit capacity: drivers have legal freedom to leave but economic impossibility of doing so (trapped); skilled drivers have economic capacity to leave but algorithmic friction and account management costs (constrained); the platform has infinite optionality (arbitrage); the union has some leverage but faces deactivation threat (constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   The platform operator is the beneficiary: they extract rent through algorithmic control of task assignment, rating stratification, and price-setting. Directionality for the platform is d ≈ 0.0 (full beneficiary), producing negative χ (subsidy flow). Dependent drivers are victims: they experience high costs (income volatility, deactivation threat, labor exhaustion) and no compensation mechanism. Directionality for dependent drivers is d ≈ 1.0 (full target), producing maximum χ (extraction flow). Skilled multi-app drivers have mixed directionality (d ≈ 0.6): they benefit from task-matching efficiency and reduced search costs but experience suppression and earning volatility, placing them partway between beneficiary and target. The driver coalition occupies d ≈ 0.5-0.6 territory: they coordinate some gains (scheduling predictability when they mount sufficient pressure) but remain suppressed relative to the platform's gains. The platform's exit options (arbitrage) make its d resilient to suppression changes — even if regulation hardens, the platform can exit into autonomous vehicles or geographic expansion, keeping d low and χ negative. Dependent drivers' exit options (trapped) make their d maximum regardless of outside-world change — they cannot exit even if regulation improves, because structural alternatives don't exist. Skilled drivers' exit options (constrained) give them middle d: they could leave but at high cost, so d settles around 0.55-0.65 depending on alternatives' actual cost. The directionality overrides are not needed here: the structural derivation from beneficiary/victim + exit correctly captures the differential experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY SIGNATURE: The original mandate of algorithmic platforms was 'efficient matching of supply and demand, reducing friction in temporary labor markets.' At t0 (year 0), this mandate was live: the platform did reduce search friction, increased utilization of driver capacity, and provided genuine alternatives to traditional taxi labor. By t5, the mandate has partly died: efficiency gains are no longer genuine (algorithmic pricing now extracts most efficiency gains as platform rent rather than passing them to drivers), alternatives are collapsing (the platform is the only viable option in many markets), and the labor-market friction reduction is now a fiction (drivers face higher behavioral friction — deactivation threat, rating anxiety, income volatility — than before algorithmic management). The constraint's persistence is explained by mandatrophy: the platform's authority is no longer grounded in efficiency (the mandate that birthed it) but in consolidated market power and path dependency (drivers have no exit, so the platform retains power even after efficiency gains are exhausted). This is the classic mandatrophy pattern: a coordination mechanism (matching) that justified temporary control becomes a pretext for rent extraction once the original problem is solved or the solver becomes entrenched. The remedy would be restoring the mandate — either by (1) opening the algorithmic system to multi-platform arbitrage (regulatory interoperability), (2) reclassifying drivers as employees with labor protections, or (3) building public-utility alternatives. The scaffold perspective (regulatory authority) is attempting exactly this remedy through mandatrophy resolution: imposing algorithmic transparency and classification changes that restore the original efficiency mandate or establish new mandates (worker protection, fair compensation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_threshold_ambiguity,
    'At what percentage of driver income-from-platform does the exit condition shift from ''constrained'' to ''trapped''?',
    'Longitudinal income replacement analysis; survey of actual alternative income sources for drivers at different tenure levels and in different geographic markets; cost-benefit analysis of retraining and platform switching by cohort',
    'If threshold < 50% of income: majority of drivers classified as ''constrained'', reducing suppression effect and lowering chi. If threshold > 80%: most drivers classified as ''trapped'', raising suppression and chi substantially, pushing more perspectives toward snare. The classification boundary is empirically resolvable but contingent on labor market conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_threshold_ambiguity, empirical, 'Income dependency threshold for exit classification shift').

omega_variable(
    algorithmic_substitution_path,
    'Is the platform''s consolidation trajectory toward autonomous vehicle replacement, or toward deepening driver dependence through algorithmic control?',
    'Analysis of platform investment patterns: R&D spending on autonomous technology vs. algorithmic management infrastructure; public statements and patent filings; market analysis of autonomous vehicle deployment timelines vs. driver base expansion or contraction',
    'If toward autonomous replacement: the constraint has a genuine sunset (drivers become obsolete, suppression mechanism fails). If toward algorithmic deepening: extraction mechanism intensifies without eventual release. Classification of scaffold vs. piton perspectives depends heavily on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_substitution_path, empirical, 'Platform technology trajectory: autonomous replacement or algorithmic deepening').

omega_variable(
    regulatory_capture_risk,
    'Will platform industry capture regulatory bodies, reducing the regulatory authority''s actual constraints and converting the scaffold into a false sunset?',
    'Tracking of regulatory outcomes: whether algorithmic disclosure mandates are enforceable; whether classification status decisions favor platforms or workers; comparative analysis across jurisdictions (EU, California, other states) where regulatory variance exists; analysis of regulatory agency staffing and funding relative to platform lobbying power',
    'If capture occurs: scaffold perspective overstates actual regulatory constraint; effective suppression remains high despite formal regulation; constraint persists as tangled_rope or snare. If regulation bites: scaffold sunset is real, extracted rents are recovered, platform shifts to managed-labor model with reduced extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether regulatory authority will remain independent or captured').

omega_variable(
    multi_platform_switching_feasibility,
    'Do multi-app strategies genuinely reduce driver dependence on any single platform, or does algorithmic stratification prevent most drivers from achieving sustainable multi-platform income?',
    'Cohort analysis of multi-app driver income variance and stability over time; tracking of platform algorithmic policies toward multi-app users; cost-benefit analysis of managing multiple accounts (app switching friction, account suspension risk); comparative income stability for single-app vs. multi-app drivers by skill level',
    'If multi-app switching is viable: exit_options for many drivers should be ''mobile'' rather than ''constrained'' or ''trapped'', reducing overall suppression. If algorithmic stratification prevents it: most drivers remain monopsony-dependent on the dominant platform, raising suppression and chi.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_platform_switching_feasibility, empirical, 'Whether multi-app switching reduces platform dependence or remains infeasible for most drivers').

omega_variable(
    extractive_intent_ambiguity,
    'Are the platform''s algorithmic tightening mechanisms designed to extract maximum rent, or are they a side effect of optimizing task-matching efficiency under competitive pressure?',
    'Comparative analysis of algorithmic design choices across platforms: platforms with explicit rent-extraction goals vs. those optimizing efficiency. Historical analysis of algorithmic changes: were they motivated by profitability (raising extraction) or by operational efficiency or regulatory compliance? Counterfactual: could equivalent efficiency be achieved with lower suppression?',
    'If designed for extraction: tangled_rope classification holds, extraction metrics are descriptively accurate. If side effect of efficiency: constraint might reclassify as rope with high theater_ratio (efficiency theater masking actual extraction). The beneficiary/victim framing depends on intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractive_intent_ambiguity, empirical, 'Whether algorithmic tightening is intentional extraction or efficiency side effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_platform_algorithmic_management, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_algo_tr_t0, gig_platform_algorithmic_management, theater_ratio, 0, 0.62).
narrative_ontology:measurement(gig_algo_tr_t1, gig_platform_algorithmic_management, theater_ratio, 1, 0.61).
narrative_ontology:measurement(gig_algo_tr_t2, gig_platform_algorithmic_management, theater_ratio, 2, 0.6).
narrative_ontology:measurement(gig_algo_tr_t3, gig_platform_algorithmic_management, theater_ratio, 3, 0.59).
narrative_ontology:measurement(gig_algo_tr_t4, gig_platform_algorithmic_management, theater_ratio, 4, 0.58).
narrative_ontology:measurement(gig_algo_tr_t5, gig_platform_algorithmic_management, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(gig_algo_be_t0, gig_platform_algorithmic_management, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gig_algo_be_t1, gig_platform_algorithmic_management, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(gig_algo_be_t2, gig_platform_algorithmic_management, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(gig_algo_be_t3, gig_platform_algorithmic_management, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(gig_algo_be_t4, gig_platform_algorithmic_management, base_extractiveness, 4, 0.66).
narrative_ontology:measurement(gig_algo_be_t5, gig_platform_algorithmic_management, base_extractiveness, 5, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gig_algo_su_t0, gig_platform_algorithmic_management, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(gig_algo_su_t1, gig_platform_algorithmic_management, suppression_requirement, 1, 0.6).
narrative_ontology:measurement(gig_algo_su_t2, gig_platform_algorithmic_management, suppression_requirement, 2, 0.66).
narrative_ontology:measurement(gig_algo_su_t3, gig_platform_algorithmic_management, suppression_requirement, 3, 0.7).
narrative_ontology:measurement(gig_algo_su_t4, gig_platform_algorithmic_management, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(gig_algo_su_t5, gig_platform_algorithmic_management, suppression_requirement, 5, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=5
narrative_ontology:measurement(gig_algo_grid_01, gig_platform_algorithmic_management, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(gig_algo_grid_02, gig_platform_algorithmic_management, accessibility_collapse(class), 5, 0.62).
narrative_ontology:measurement(gig_algo_grid_03, gig_platform_algorithmic_management, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(gig_algo_grid_04, gig_platform_algorithmic_management, accessibility_collapse(individual), 5, 0.85).
narrative_ontology:measurement(gig_algo_grid_05, gig_platform_algorithmic_management, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(gig_algo_grid_06, gig_platform_algorithmic_management, accessibility_collapse(organizational), 5, 0.71).
narrative_ontology:measurement(gig_algo_grid_07, gig_platform_algorithmic_management, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(gig_algo_grid_08, gig_platform_algorithmic_management, accessibility_collapse(structural), 5, 0.51).
narrative_ontology:measurement(gig_algo_grid_09, gig_platform_algorithmic_management, resistance(class), 0, 0.38).
narrative_ontology:measurement(gig_algo_grid_10, gig_platform_algorithmic_management, resistance(class), 5, 0.48).
narrative_ontology:measurement(gig_algo_grid_11, gig_platform_algorithmic_management, resistance(individual), 0, 0.28).
narrative_ontology:measurement(gig_algo_grid_12, gig_platform_algorithmic_management, resistance(individual), 5, 0.18).
narrative_ontology:measurement(gig_algo_grid_13, gig_platform_algorithmic_management, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(gig_algo_grid_14, gig_platform_algorithmic_management, resistance(organizational), 5, 0.61).
narrative_ontology:measurement(gig_algo_grid_15, gig_platform_algorithmic_management, resistance(structural), 0, 0.22).
narrative_ontology:measurement(gig_algo_grid_16, gig_platform_algorithmic_management, resistance(structural), 5, 0.31).
narrative_ontology:measurement(gig_algo_grid_17, gig_platform_algorithmic_management, stakes_inflation(class), 0, 0.41).
narrative_ontology:measurement(gig_algo_grid_18, gig_platform_algorithmic_management, stakes_inflation(class), 5, 0.54).
narrative_ontology:measurement(gig_algo_grid_19, gig_platform_algorithmic_management, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(gig_algo_grid_20, gig_platform_algorithmic_management, stakes_inflation(individual), 5, 0.79).
narrative_ontology:measurement(gig_algo_grid_21, gig_platform_algorithmic_management, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(gig_algo_grid_22, gig_platform_algorithmic_management, stakes_inflation(organizational), 5, 0.58).
narrative_ontology:measurement(gig_algo_grid_23, gig_platform_algorithmic_management, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(gig_algo_grid_24, gig_platform_algorithmic_management, stakes_inflation(structural), 5, 0.42).
narrative_ontology:measurement(gig_algo_grid_25, gig_platform_algorithmic_management, suppression(class), 0, 0.38).
narrative_ontology:measurement(gig_algo_grid_26, gig_platform_algorithmic_management, suppression(class), 5, 0.52).
narrative_ontology:measurement(gig_algo_grid_27, gig_platform_algorithmic_management, suppression(individual), 0, 0.58).
narrative_ontology:measurement(gig_algo_grid_28, gig_platform_algorithmic_management, suppression(individual), 5, 0.76).
narrative_ontology:measurement(gig_algo_grid_29, gig_platform_algorithmic_management, suppression(organizational), 0, 0.44).
narrative_ontology:measurement(gig_algo_grid_30, gig_platform_algorithmic_management, suppression(organizational), 5, 0.62).
narrative_ontology:measurement(gig_algo_grid_31, gig_platform_algorithmic_management, suppression(structural), 0, 0.32).
narrative_ontology:measurement(gig_algo_grid_32, gig_platform_algorithmic_management, suppression(structural), 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_platform_algorithmic_management, resource_allocation).
narrative_ontology:affects_constraint(gig_platform_algorithmic_management, algorithmic_pricing_wage_suppression).
narrative_ontology:affects_constraint(gig_platform_algorithmic_management, driver_deactivation_threat).
narrative_ontology:affects_constraint(gig_platform_algorithmic_management, multi_app_arbitrage_closure).

% DUAL FORMULATION NOTE:
% The algorithmic management constraint is upstream of specific pricing and deactivation mechanisms. Wage suppression, deactivation threat, and multi-app arbitrage closure are downstream constraints that manifest the general algorithmic control structure. Each downstream constraint has its own ε and perspectives; the tightening dynamics of algorithmic management create the conditions for extraction at each downstream point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
