% ============================================================================
% CONSTRAINT STORY: platform_flexibility_precarity_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_flexibility_precarity_tradeoff, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: platform_flexibility_precarity_tradeoff
 *   human_readable: Platform Flexibility-Precarity Tradeoff in Gig Economy
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The platform gig economy presents a structural tradeoff between income
 *   flexibility and long-term economic security. Platform companies like
 *   Meituan, Uber, DoorDash, and Deliveroo offer workers the ability to set
 *   their own schedules and supplement income, solving genuine coordination
 *   problems around labor supply elasticity and demand matching. However,
 *   this flexibility is achieved through employment classification as
 *   independent contractors, which structurally excludes workers from
 *   traditional labor protections (minimum wage, overtime, benefits,
 *   unemployment insurance, retirement contributions). The constraint
 *   exhibits tangled rope characteristics: both the coordination function
 *   (flexible scheduling, income supplementation) and the extraction
 *   mechanism (algorithmic wage pressure, benefit exclusion, retirement
 *   insecurity) are structurally real and operate simultaneously through the
 *   same institutional arrangement. The 48% of Meituan riders working fewer
 *   than 30 days per year reflects both the flexibility the model enables and
 *   the precarity it generates — workers cycle in and out based on
 *   desperation rather than choice. The 42.3% retirement confidence rate
 *   among platform workers compared to traditional employment reveals the
 *   long-term security cost of short-term flexibility. The constraint
 *   requires active enforcement through misclassification litigation,
 *   regulatory arbitrage, and algorithmic management systems that prevent
 *   worker organization.
 *
 * KEY AGENTS:
 *   - Platform Companies: Primary beneficiary (institutional/arbitrage) — capture labor cost savings, regulatory arbitrage, data rents, and market power through independent contractor model
 *   - Flexible Workers (Long-term Security): Primary victim (powerless/trapped for tenure-locked; moderate/constrained for supplemental income) — bear income volatility, lack of benefits, retirement insecurity, algorithmic control
 *   - Workers with Alternative Income: Secondary beneficiary (moderate/constrained) — genuinely benefit from scheduling flexibility while bearing some extraction costs
 *   - Consumers: Mixed position (moderate/constrained) — benefit from lower prices and convenience, indirectly bear costs through safety net externalization
 *   - Traditional Labor Protections: Victim (abstract institutional good) — eroded by platform model's regulatory arbitrage
 *   - Labor Rights Coalition: Organized agents (organized/constrained) — building portable benefits frameworks and regulatory reforms with scaffold logic
 *   - Traditional Employment Classification System: Institutional actor (institutional/constrained) — maintains performative classification ritual while functional capacity has degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_flexibility_precarity_tradeoff, 0.58).
domain_priors:suppression_score(platform_flexibility_precarity_tradeoff, 0.62).
domain_priors:theater_ratio(platform_flexibility_precarity_tradeoff, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_flexibility_precarity_tradeoff, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_flexibility_precarity_tradeoff, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(platform_flexibility_precarity_tradeoff, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(platform_flexibility_precarity_tradeoff, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(platform_flexibility_precarity_tradeoff, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_flexibility_precarity_tradeoff, tangled_rope).
narrative_ontology:human_readable(platform_flexibility_precarity_tradeoff, "Platform Flexibility-Precarity Tradeoff in Gig Economy").
narrative_ontology:topic_domain(platform_flexibility_precarity_tradeoff, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(platform_flexibility_precarity_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_flexibility_precarity_tradeoff, platform_companies).
narrative_ontology:constraint_beneficiary(platform_flexibility_precarity_tradeoff, workers_with_alternative_income).
narrative_ontology:constraint_beneficiary(platform_flexibility_precarity_tradeoff, consumers_of_platform_services).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, flexible_workers_long_term_security).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, traditional_labor_protections).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, retirement_security_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(platform_flexibility_precarity_tradeoff, supplemental_income_workers).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, tenure_locked_riders).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, supplemental_income_workers).
narrative_ontology:constraint_victim(platform_flexibility_precarity_tradeoff, consumers_of_platform_services).
narrative_ontology:constraint_vindicates(platform_flexibility_precarity_tradeoff, labor_market_flexibility_doctrine).
narrative_ontology:constraint_vindicates(platform_flexibility_precarity_tradeoff, independent_contractor_efficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of the labor arrangement through algorithmic management, pricing structures, and independent contractor classification. Capture labor cost savings, regulatory arbitrage, and data rents. Can exit to other business models or geographies at will. Experience the arrangement as efficient market-making.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, platform_companies, agenda_setter,
    institutional, immediate, arbitrage, global).

% Workers with no alternative income source, trapped in platform work by lack of traditional employment options. Bear income volatility, no benefits, no retirement security, algorithmic control. The 48% turnover within 30 days reflects desperation cycling — exit when they find anything better, return when they have no alternative. Cannot organize effectively due to atomization.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, tenure_locked_riders, payer,
    powerless, biographical, trapped, national).

% Use platform as secondary income source alongside traditional employment or family support. Genuinely benefit from flexibility (can work around other commitments) but also bear extraction through algorithmic wage pressure and lack of protections. Mixed experience: coordination solves scheduling problem but extraction is also real.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, supplemental_income_workers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(platform_flexibility_precarity_tradeoff, supplemental_income_workers, payer).

% Benefit from lower prices and convenience enabled by platform labor model, but also bear indirect costs through erosion of labor standards and social safety net externalization. The coordination function is genuine (on-demand service delivery) but low prices partly reflect cost-shifting from platforms to workers and public systems. Constrained exit because platform services have become infrastructure.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, consumers_of_platform_services, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(platform_flexibility_precarity_tradeoff, consumers_of_platform_services, payer).

% Organized labor advocates, worker centers, and policy reformers building portable benefits legislation, sectoral bargaining frameworks, and algorithmic accountability regulations. See the current arrangement as temporary — the pure independent contractor model is politically unsustainable as gig work scales. Working to preserve flexibility while restoring protections.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, labor_rights_coalition, observer,
    organized, generational, constrained, national).

% Regulatory agencies and courts maintaining the employee/independent contractor classification ritual through ABC tests and economic realities tests. The tests produce inconsistent results and fail to capture platform work's actual structure. The system persists through institutional inertia while its functional capacity to protect workers or provide regulatory clarity has degraded.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, traditional_employment_regulators, observer,
    institutional, biographical, constrained, national).

% Abstract institutional good representing the system of minimum wage, overtime, benefits, unemployment insurance, and retirement contributions. Eroded by platform model's regulatory arbitrage. Not a real-world actor but a structural victim of the constraint's operation.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, traditional_labor_protections, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(platform_flexibility_precarity_tradeoff, traditional_labor_protections).

% Abstract institutional good representing long-term economic security infrastructure. The 42.3% retirement confidence rate among platform workers reveals the constraint's long-term cost. Not a real-world actor but a structural victim as platform work externalizes retirement costs to public safety nets.
narrative_ontology:constraint_stakeholder(platform_flexibility_precarity_tradeoff, retirement_security_systems, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(platform_flexibility_precarity_tradeoff, retirement_security_systems).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The platform model solves genuine coordination problems: matching elastic labor supply with variable demand, enabling workers to set their own schedules, allowing income supplementation around other commitments, and providing on-demand service delivery to consumers. These coordination functions are structurally real.
% TRANSFER_FUNCTION: The arrangement transfers labor cost savings, regulatory compliance costs, and long-term security costs from platform companies to workers and public safety nets. It transfers convenience and lower prices to consumers. It transfers data and market power to platforms. Money flows from consumers to platforms; risk flows from platforms to workers; long-term costs flow from platforms to public systems.
% ABSENT_VOICES: Workers who have been deactivated by algorithmic systems without appeal are excluded from the conversation — they cannot organize or contest their exclusion. Future retirees who will depend on public assistance due to inadequate platform-era savings are not in the room. Traditional labor unions have limited access to atomized platform workers. Policymakers in jurisdictions where platforms have not yet scaled lack representation in the current arrangement.
% DISAPPEARANCE_RATIONALE: If the platform flexibility-precarity tradeoff disappeared overnight, the world would rearrange substantially. Workers would need to find alternative income sources (traditional employment, other platforms, or exit the labor market). Consumers would lose on-demand service access and pay higher prices. Platform companies would need to restructure their labor models or exit markets. The arrangement is not a natural fact — it depends on specific regulatory classifications, algorithmic management systems, and market structures that could be changed.
% FOUNDING_PROBLEM: The founding problem was labor market rigidity in traditional employment: workers with caregiving responsibilities, students, retirees, and others seeking supplemental income faced high barriers to part-time or flexible work. Traditional employment's fixed schedules, minimum hour requirements, and geographic constraints prevented many people from participating in the labor market. Platforms claimed to solve this by enabling anyone with a smartphone to earn income on their own schedule.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some workers (particularly supplemental income workers) attest that the founding problem remains live — traditional employment is still rigid and platform work provides valuable flexibility. Labor economists and worker advocates contest this, arguing that the founding problem has been solved for some workers (genuine flexibility beneficiaries) but the solution has been over-applied to workers who need stability more than flexibility, and that the extraction costs now exceed the coordination benefits for many workers. The status is genuinely contested rather than clearly live or dead.
narrative_ontology:disappearance_verdict(platform_flexibility_precarity_tradeoff, world_rearranges).
narrative_ontology:founding_problem_status(platform_flexibility_precarity_tradeoff, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TENURE-LOCKED RIDER (SNARE) — Worker with no alternative income source, trapped in platform work by lack of traditional employment options. Experiences maximum extraction: income volatility, no benefits, no retirement security, algorithmic control. The 48% turnover rate within 30 days reflects not worker choice but desperation cycling — workers exit when they find anything better, return when they have no alternative. Cannot organize effectively due to atomization and algorithmic management.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUPPLEMENTAL INCOME WORKER (TANGLED ROPE) — Worker using platform as secondary income source alongside traditional employment or family support. Genuinely benefits from flexibility (can work around other commitments) but also bears extraction through algorithmic wage pressure and lack of protections. Mixed experience: coordination function is real (solves scheduling problem) but extraction is also real (platform captures surplus through information asymmetry and market power).
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM COMPANY (ROPE) — Primary beneficiary. Experiences the arrangement as pure coordination: matching workers with demand, providing technology infrastructure, enabling flexible labor supply. Extraction runs toward this agent (labor cost arbitrage, regulatory arbitrage, data capture) but from their structural position the constraint appears as efficient market-making. Can exit to other business models or geographies at will.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER (TANGLED ROPE) — Benefits from lower prices and convenience enabled by platform labor model, but also bears indirect costs through erosion of labor standards and social safety net externalization. The coordination function is genuine (on-demand service delivery) but the low prices partly reflect cost-shifting from platforms to workers and public systems. Constrained exit because platform services have become infrastructure.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (SCAFFOLD) — Organized labor advocates, worker centers, and policy reformers see the current arrangement as temporary: portable benefits legislation, sectoral bargaining frameworks, and algorithmic accountability regulations are being built to preserve flexibility while restoring protections. Sunset logic: the pure independent contractor model is politically unsustainable as gig work scales; hybrid models (California AB5, EU Platform Work Directive) represent transition to regulated flexibility. Estimated sunset: 5-15 years for new frameworks to stabilize.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL EMPLOYMENT CLASSIFICATION (PITON) — The employee/independent contractor binary was designed for industrial-era work relationships and has atrophied in the platform economy. Regulatory agencies and courts maintain the classification ritual (ABC tests, economic realities tests) but the tests produce inconsistent results and fail to capture platform work's actual structure. The system persists through institutional inertia while its functional capacity to protect workers or provide regulatory clarity has degraded. Theater ratio reflects performative compliance and enforcement.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the platform model genuinely solves coordination problems (matching supply and demand, enabling flexible work arrangements) while simultaneously extracting rents through information asymmetry, algorithmic control, and regulatory arbitrage. Both functions are structurally real. The 42.3% retirement confidence figure and 48% short-tenure rate are not bugs but features: the model requires a reserve army of precarious workers to maintain on-demand availability. This is the claimed type — the analytical perspective that the base metrics and structural data should produce.
constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_flexibility_precarity_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_flexibility_precarity_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_flexibility_precarity_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_flexibility_precarity_tradeoff, TR),
    TR >= 0.70.

:- end_tests(platform_flexibility_precarity_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Platform companies capture significant rents through information asymmetry (algorithmic pricing, demand forecasting), market power (network effects, geographic monopolies), and regulatory arbitrage (avoiding employer obligations). Workers bear income volatility, lack of benefits, and long-term insecurity. However, extraction is not maximal because some workers genuinely benefit from flexibility and the coordination function is real. The value reflects that extraction and coordination coexist. Suppression (0.62): Moderate-high. Workers face significant barriers to exit and organization: algorithmic management prevents collective action, deactivation without appeal creates compliance pressure, market concentration limits alternative platforms, lack of traditional employment options traps some workers. But suppression is not total — workers can and do exit to traditional employment when available, multi-apping provides some leverage, and labor organizing is emerging despite barriers. Theater ratio (0.48): Moderate. The independent contractor classification involves substantial performance: platforms maintain legal fictions of worker autonomy (can set own schedule, choose which orders to accept) while algorithmic systems effectively control work through acceptance rate tracking, dynamic pricing, and deactivation. Regulatory compliance is partly theatrical (worker agreements, training modules) while actual protection is minimal. However, theater is not dominant — the flexibility is partly real and the coordination function operates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural arrangement produces radically different experiences based on the agent's position. Platform companies see efficient market-making (rope). Tenure-locked workers see a trap with no exit (snare). Supplemental income workers see mixed benefits and costs (tangled rope). The labor rights coalition sees a temporary problem being solved (scaffold). The traditional regulatory system sees its own obsolescence (piton). The analytical observer sees irreducible hybridity (tangled rope). The gap is not a measurement error — it reflects that flexibility genuinely coordinates for some agents while extracting from others, and the same institutional structure performs both functions simultaneously. The perspectival gap IS the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are primary beneficiaries with arbitrage-level exit options — they experience the constraint as pure coordination (rope classification) because extraction flows toward them. Tenure-locked workers with no alternative income are primary victims with trapped exit options — they experience maximum extraction (snare classification) because they cannot leave and bear full costs. Supplemental income workers are mixed — they benefit from flexibility but also bear extraction costs, producing tangled rope classification from their constrained position. Consumers benefit from lower prices but indirectly subsidize the model through safety net externalization, also producing tangled rope. The labor rights coalition sees a temporary problem with a sunset (scaffold) because they have agency to build alternative frameworks. The traditional classification system sees its own degraded ritual (piton) because the employee/contractor binary no longer maps to platform work structure. The analytical observer sees both coordination and extraction as structurally real (tangled rope), which is the claimed type.
 *
 * MANDATROPHY ANALYSIS:
 *   The platform flexibility-precarity tradeoff resolves the mandatrophy by demonstrating that tangled rope is the structurally correct classification when both coordination and extraction are irreducibly present. The constraint is not mislabeled coordination (pure rope) because the extraction is real and substantial — workers bear genuine costs through income volatility, benefit exclusion, and retirement insecurity. The constraint is not mislabeled extraction (pure snare) because the coordination function is also real — flexible scheduling solves genuine problems for workers with other commitments, and demand-matching creates value. The tangled rope classification captures that both functions operate through the same mechanism: the independent contractor model enables flexibility (coordination) while excluding protections (extraction). The mandate (flexible labor supply) has not outlived its function — platforms still need elastic labor. But the extraction has accumulated as platforms have gained market power and algorithmic control has intensified. The scaffold perspective (labor rights coalition) represents the possibility of untangling: preserving coordination while reducing extraction through portable benefits and algorithmic accountability. Whether that untangling succeeds is an open empirical question (omega variables).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexibility_valuation_heterogeneity,
    'What proportion of platform workers genuinely value flexibility over stability versus accept flexibility as the only available option?',
    'Longitudinal surveys tracking worker preferences before and after platform entry; revealed preference analysis comparing platform work uptake rates across different labor market conditions; exit interviews from workers who leave platform work',
    'If high genuine preference (>60%): coordination function dominates, extraction is lower than measured. If low genuine preference (<30%): extraction dominates, workers are trapped by lack of alternatives rather than choosing flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_valuation_heterogeneity, empirical, 'Proportion of workers genuinely preferring flexibility over stability').

omega_variable(
    portable_benefits_sufficiency,
    'Can portable benefits frameworks (hour banks, multi-employer plans, universal basic income) preserve flexibility while restoring security, or does flexibility structurally require precarity?',
    'Evaluation of existing portable benefits pilots (Seattle, New York, EU experiments); comparison of worker outcomes under different regulatory regimes; modeling of cost distribution under various benefit portability schemes',
    'If portable benefits work: scaffold perspective confirmed, sunset is real, tangled rope can be untangled. If they fail: flexibility and security are structurally incompatible, current extraction is inherent to the model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(portable_benefits_sufficiency, empirical, 'Whether portable benefits can preserve flexibility while restoring security').

omega_variable(
    algorithmic_management_necessity,
    'Is algorithmic control (dynamic pricing, acceptance rate tracking, deactivation without appeal) necessary for platform coordination function or primarily an extraction mechanism?',
    'Comparison of platform performance metrics under different management regimes; analysis of worker-owned platform cooperatives; experimental manipulation of algorithmic transparency and worker autonomy',
    'If necessary for coordination: extraction is inherent to the model''s efficiency. If primarily extractive: coordination function could be preserved with less worker control, suggesting current model over-extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_management_necessity, empirical, 'Whether algorithmic control is necessary for coordination or primarily extractive').

omega_variable(
    turnover_interpretation_ambiguity,
    'Does the 48% short-tenure rate (<30 days/year) reflect worker choice (trying platform work and choosing to leave) or desperation cycling (forced exits and returns due to lack of alternatives)?',
    'Tracking individual worker trajectories across multiple platforms and traditional employment; analysis of re-entry patterns and duration between platform work stints; correlation between local unemployment rates and platform tenure distributions',
    'If choice-driven: flexibility coordination function is real, workers are exercising agency. If desperation-driven: high turnover reflects extraction severity, workers are churning through an unstable system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(turnover_interpretation_ambiguity, empirical, 'Whether high turnover reflects worker choice or desperation cycling').

omega_variable(
    retirement_security_externalization,
    'Is the 42.3% retirement confidence rate among platform workers a private risk choice or a socialized cost externalized to public safety nets?',
    'Projection of platform worker retirement outcomes under current savings rates; estimation of public assistance costs for retired platform workers; comparison with traditional employment retirement security trajectories',
    'If private risk: workers are making informed tradeoffs, extraction is lower. If externalized cost: platforms are shifting long-term costs to public systems, extraction is higher than current metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retirement_security_externalization, empirical, 'Whether low retirement confidence represents private risk or externalized public cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_flexibility_precarity_tradeoff, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_platform, platform_flexibility_precarity_tradeoff, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_regulatory_response, platform_flexibility_precarity_tradeoff, theater_ratio, 3, 0.35).
narrative_ontology:measurement(theater_classification_litigation, platform_flexibility_precarity_tradeoff, theater_ratio, 6, 0.42).
narrative_ontology:measurement(theater_current, platform_flexibility_precarity_tradeoff, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(extract_early_platform, platform_flexibility_precarity_tradeoff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_market_maturation, platform_flexibility_precarity_tradeoff, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(extract_algorithmic_intensification, platform_flexibility_precarity_tradeoff, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(extract_current, platform_flexibility_precarity_tradeoff, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(suppress_early_platform, platform_flexibility_precarity_tradeoff, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(suppress_market_concentration, platform_flexibility_precarity_tradeoff, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(suppress_algorithmic_control, platform_flexibility_precarity_tradeoff, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(suppress_current, platform_flexibility_precarity_tradeoff, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_flexibility_precarity_tradeoff, resource_allocation).
narrative_ontology:affects_constraint(platform_flexibility_precarity_tradeoff, social_safety_net_adequacy).
narrative_ontology:affects_constraint(platform_flexibility_precarity_tradeoff, retirement_security_crisis).
narrative_ontology:affects_constraint(platform_flexibility_precarity_tradeoff, algorithmic_management_labor_control).

% DUAL FORMULATION NOTE:
% The platform flexibility-precarity tradeoff is structurally distinct from but networked with broader labor market constraints. Social safety net adequacy is affected because platform work externalizes costs to public systems. Retirement security crisis is affected because platform workers have low retirement savings rates. Algorithmic management labor control is a related but distinct constraint focusing on the control mechanism rather than the flexibility-security tradeoff itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_flexibility_precarity_tradeoff, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
