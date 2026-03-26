% ============================================================================
% CONSTRAINT STORY: olympic_host_infrastructure_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_olympic_host_infrastructure_debt, []).

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
 *   constraint_id: olympic_host_infrastructure_debt
 *   human_readable: Olympic Host Infrastructure Debt Trap
 *   domain: economic_policy/urban_development
 *
 * SUMMARY:
 *   Olympic host infrastructure debt represents a structural extraction
 *   mechanism embedded in international sports governance. Host nations
 *   commit to building massive permanent facilities to meet IOC standards,
 *   incurring decades of debt servicing that benefits international
 *   organizers, construction sectors, and developers while distributing costs
 *   across all local taxpayers. The constraint exhibits high suppression
 *   through contractual lock-in and political coercion (no exit without
 *   penalty), moderate extractiveness driven by the cost-benefit asymmetry,
 *   and rising theater as actual infrastructure utility declines relative to
 *   spectacle. The constraint shifts from being partially justified (cities
 *   need some development) at onset to nakedly extractive over the 20-30 year
 *   debt servicing period as initial prestige benefits fade and genuine need
 *   for specialized Olympic facilities vanishes. Multiple institutional
 *   arrangements (IOC governance, host-nation competitive bidding,
 *   development financing) create a snare where rational individual
 *   incentives (cities want international prestige, developers want
 *   profitable contracts) produce collectively pathological outcomes
 *   (unsustainable debt, forced gentrification, service cuts).
 *
 * KEY AGENTS:
 *   - Host City Taxpayers: Primary victims (powerless/trapped) — bear multi-decade debt burden through taxation and reduced public services
 *   - Working Class Residents: Secondary victims (powerless/trapped) — face displacement through gentrification and cost-of-living increases
 *   - International Olympic Committee: Primary beneficiary (institutional/arbitrage) — extracts governance fees and broadcast value while outsourcing financial risk
 *   - Construction Contractors & Developers: Secondary beneficiaries (powerful/mobile) — capture premium pricing and land value appreciation with limited downside exposure
 *   - Host City Political Leadership: Mixed agent (organized/constrained) — experiences both prestige benefits (short-term) and debt extraction (long-term); constrained by electoral pressures and contractual obligations
 *   - Olympic Movement Institutions: Institutional persistence agent (institutional/constrained) — maintains extractive structure through inertia and prestige narratives despite functional redundancy
 *   - Reform Coalitions: Counter-agent (organized/constrained) — advocates for permanent venue models or cost-sharing frameworks with constrained leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_host_infrastructure_debt, 0.62).
domain_priors:suppression_score(olympic_host_infrastructure_debt, 0.68).
domain_priors:theater_ratio(olympic_host_infrastructure_debt, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_host_infrastructure_debt, extractiveness, 0.62).
narrative_ontology:constraint_metric(olympic_host_infrastructure_debt, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(olympic_host_infrastructure_debt, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_host_infrastructure_debt, snare).
narrative_ontology:human_readable(olympic_host_infrastructure_debt, "Olympic Host Infrastructure Debt Trap").
narrative_ontology:topic_domain(olympic_host_infrastructure_debt, "economic_policy/urban_development").

domain_priors:requires_active_enforcement(olympic_host_infrastructure_debt).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_host_infrastructure_debt, international_olympic_committee).
narrative_ontology:constraint_beneficiary(olympic_host_infrastructure_debt, construction_contractors).
narrative_ontology:constraint_beneficiary(olympic_host_infrastructure_debt, real_estate_developers).
narrative_ontology:constraint_victim(olympic_host_infrastructure_debt, host_city_taxpayers).
narrative_ontology:constraint_victim(olympic_host_infrastructure_debt, public_services_funding).
narrative_ontology:constraint_victim(olympic_host_infrastructure_debt, local_working_class_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOST CITY TAXPAYERS (SNARE) — Trapped by legally binding Olympic host agreements. Cannot exit without massive penalties and reputational damage. Bears full cost of infrastructure debt through decades of tax burden. Suppression is high: contractual obligation, sunk cost fallacy, and political pressure prevent exit. Zero agency; maximum extraction.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WORKING CLASS RESIDENTS (SNARE) — Gentrification from Olympic development and cost-of-living increases from debt servicing force displacement. Trapped by housing market dynamics and lack of affordable alternatives. Bears extraction through loss of place and community. Multi-generational impact through inherited debt burden.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: INTERNATIONAL OLYMPIC COMMITTEE (ROPE) — Experiences the constraint as pure coordination: standardized venues, facilities, and infrastructure requirements that multiple host cities successfully coordinate. IOC extracts through governance fees, broadcast rights, and sponsorship while outsourcing financial risk to host nations. No suppression experienced at IOC level; no exit barriers. Pure beneficiary with arbitrage options.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTRUCTION & REAL ESTATE (TANGLED ROPE) — Benefits massively from Olympic contracts: guaranteed work, premium pricing, land value appreciation. Also faces some extraction through regulatory compliance and competitive bidding pressure. Mobile agents with substantial agency — can pursue other projects but Olympic opportunities are lucrative. Mixed coordination (building needed infrastructure) and extraction (capturing surplus value).
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: OLYMPIC MOVEMENT INSTITUTIONAL LOGIC (PITON) — The constraint persists through institutional inertia and theatrical prestige. The Olympics' core coordination function (bringing nations together) could be achieved with much lower infrastructure cost through rotating permanent venues or virtual components. Instead, the extravagant permanent-facility requirement persists because the spectacle and national prestige narratives drive political competition for hosting rights. Theater ratio is moderate-high: the visible spectacle of opening ceremonies and state-of-the-art venues masks the functional redundancy of most infrastructure.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORM COALITIONS (SCAFFOLD) — Organized activists, economists, and city planners advocate for rotating permanent venues (reducing per-host construction cost) or reduced infrastructure requirements. This perspective sees the constraint as temporary and solvable through governance reform. Exit path exists: structural changes to Olympic host selection and facility requirements. Sunset clause: If permanent venue model or cost-sharing frameworks adopted, the per-host extraction mechanism weakens dramatically.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: HOST CITY POLITICAL LEADERSHIP (TANGLED ROPE) — Benefits from short-term prestige, construction-sector employment, and international attention. Also bears extraction through long-term debt and constrained budget priorities. Constrained exit: cannot walk away without electoral consequences. Experience a mix of genuine coordination (managing a complex international event) and asymmetric extraction (IOC captures upside, host city captures downside).
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER - FALSE SUMMIT (MOUNTAIN) — From a distant analytical perspective, the constraint appears immutable: international events require infrastructure, nations compete for prestige, and debt is the inevitable cost of development. This naturalizes what is actually a contingent institutional arrangement driven by political competition and extractive IOC governance. The engine's false summit detector will flag this perspective as naturalization of extractive structures as laws of nature. The constraint is changeable — permanent venues, cost-sharing, or reduced facility requirements could dramatically lower host-nation burden.
constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_host_infrastructure_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_host_infrastructure_debt, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(olympic_host_infrastructure_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(olympic_host_infrastructure_debt, TR),
    TR >= 0.70.

:- end_tests(olympic_host_infrastructure_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high, reflecting structural cost asymmetry. Host nations bear full infrastructure and debt-servicing costs while IOC and multinational contractors capture disproportionate benefits. The value is not higher (e.g., 0.78) because some infrastructure serves legitimate city needs and because prestige benefits exist (even if short-lived). Over the 30-year interval, extractiveness rises from 0.35 to 0.68 as the proportion of justified infrastructure decreases and debt service becomes the dominant burden. Suppression (0.68): High, reflecting contractual lock-in, political coercion, and reputational costs of withdrawal. Host nations cannot exit Olympic obligations without triggering IOC penalties and international embarrassment. Limited alternatives exist for achieving comparable international prestige at lower cost. Theater ratio (0.58): Moderate. The opening ceremony spectacle and new-venue imagery constitute substantial performative content, but genuine coordination work (managing thousands of athletes, complex logistics, security) is real. The ratio rises post-event as the performing arts infrastructure (stadiums, ceremonies) outlive their functional utility but remain symbolically important. Reform-era perspectives show theater declining as permanent-venue or cost-reduction models reduce the performative emphasis.
 *
 * PERSPECTIVAL GAP:
 *   Snare perspective from powerless agents with trapped exit options (d ≈ 0.95) sees pure extraction: contractually locked, unable to exit, bearing full cost. Rope perspective from institutional beneficiaries with arbitrage options (d ≈ 0.05) sees coordination: achieving international event goals at reasonable cost to them. Tangled Rope from political leadership (d ≈ 0.60) sees both: genuine coordination work mixed with significant fiscal burden they cannot escape. Piton perspective sees functional degradation: Olympic facilities lose utility after event, but the constraint persists through institutional identity (hosting the Olympics becomes identity marker for cities). Scaffold perspective sees temporary problem with solution path: reform coalitions point to permanent venues or cost-sharing models that would restructure the extraction. False summit perspective naturalizes the constraint as inherent to international competition, masking that IOC governance choices (not laws of nature) create the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationship to the extraction flow. IOC (institutional/arbitrage) occupies d ≈ 0.05 — full beneficiary, minimal extraction experienced. Contractors (powerful/mobile) occupy d ≈ 0.25 — secondary beneficiaries with some constraints. Host taxpayers (powerless/trapped) occupy d ≈ 0.95 — full targets with maximum extraction. Working class residents (powerless/trapped but also displaced) occupy d ≈ 0.98 — compound extraction (debt burden plus displacement). Political leadership (organized/constrained) occupies d ≈ 0.60 — mixed position with short-term prestige benefits offset by long-term fiscal extraction. The sigmoid f(d) transforms these d values to effective extraction multipliers: 0.05 → -0.12 (IOC experiences negative χ — constraint subsidizes them), 0.25 → 0.02 (contractors experience minimal effective extraction despite nominal participation), 0.95 → 1.42 (powerless taxpayers experience maximum effective extraction), 0.60 → 0.87 (political leadership experiences significant but mixed extraction). Scope modifier σ(S) applies: local scope (σ=0.8) modulates base χ downward for individual taxpayers, but global IOC scope (σ=1.2) modulates their χ upward.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.62 > 0.46): The constraint resolves mandatrophy by distinguishing genuine coordination (managing complex international events, building infrastructure with positive externalities) from extractive asymmetry (IOC capturing upside, host nations capturing downside, power concentration enabling unequal contracts). The classification as Snare from powerless perspective is confirmed by structural data: suppression is high (contractual lock-in, exit costs), extractiveness is significant (cost asymmetry), and victims have no agency. The Rope perspective from IOC is confirmed: they experience minimal suppression, extract significant value, and face no exit barriers. The Tangled Rope perspective from political leadership is confirmed: genuine coordination work (hosting massive event) coexists with asymmetric extraction (long-term debt, constrained budgets). The Piton perspective is confirmed: theater ratio rises over time as functional utility declines but institutional identity and prestige narratives maintain the constraint. The mandatrophy is resolved: this is not 'coordination wrongly labeled extraction' nor 'extraction wrongly labeled coordination' — it is structurally mixed, with different perspectives experiencing different portions of the mix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_infrastructure_utility,
    'What proportion of Olympic infrastructure would be built by the host city in the absence of Olympic hosting obligations?',
    'Comparison of pre-Olympic city development plans with actual Olympic construction; cost-benefit analysis of permanent venues vs one-time use facilities; analysis of maintenance and utilization rates post-Olympics',
    'If high proportion justified by city needs: debt is partially legitimate infrastructure investment (Tangled Rope severity reduced). If low proportion: most infrastructure is pure extractive overhead (Snare classification confirmed). Directly affects ε estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_infrastructure_utility, empirical, 'Whether Olympic infrastructure serves genuine city needs or is purely extractive').

omega_variable(
    ioc_extraction_mechanism_opacity,
    'To what degree does the IOC actively extract surplus value vs passively benefit from host-nation competition for prestige?',
    'Forensic analysis of IOC governance, broadcast revenue distribution, sponsor payment structures, and host-nation contractual obligations; comparison of IOC revenue to host-nation expenditure ratios across Olympics',
    'If active extraction (deliberate contract design that concentrates IOC benefit): classification remains Snare with high ε. If passive benefit (host nations voluntarily overspend for prestige): classification may shift to Tangled Rope with mixed motivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ioc_extraction_mechanism_opacity, empirical, 'Whether IOC actively extracts surplus or benefits passively from host competition').

omega_variable(
    permanent_venue_political_feasibility,
    'Is transition to permanent Olympic venues (rotating or fixed locations) politically feasible within 2-3 Olympic cycles?',
    'Assessment of IOC reform initiatives, voting patterns in host-city selection, public sentiment analysis, cost-saving quantification, and uptake of alternative models (e.g., Los Angeles'' 2028 strategy using existing venues)',
    'If feasible: Scaffold perspective is structural (sunset clause is real). If infeasible: reform is aspirational; constraint persists indefinitely. Affects whether classification should remain Snare or transition toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanent_venue_political_feasibility, empirical, 'Political feasibility of permanent Olympic venue models').

omega_variable(
    intergenerational_debt_incidence,
    'How much of Olympic-related debt is paid by the generation that experienced the event vs subsequent generations?',
    'Longitudinal analysis of debt servicing timelines, retirement patterns of taxpayers who voted for hosting, and fiscal analysis of Olympic-city budgets 10-20 years post-event',
    'If primary generators do not bear primary costs: extraction is strongly intergenerational (increases moral severity). Affects how to weight Snare classification and directionality for future taxpayers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_debt_incidence, empirical, 'Generational incidence of Olympic debt burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_host_infrastructure_debt, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olympic_tr_t0, olympic_host_infrastructure_debt, theater_ratio, 0, 0.4).
narrative_ontology:measurement(olympic_tr_t2, olympic_host_infrastructure_debt, theater_ratio, 2, 0.48).
narrative_ontology:measurement(olympic_tr_t4, olympic_host_infrastructure_debt, theater_ratio, 4, 0.58).
narrative_ontology:measurement(olympic_tr_t8, olympic_host_infrastructure_debt, theater_ratio, 8, 0.62).
narrative_ontology:measurement(olympic_tr_t16, olympic_host_infrastructure_debt, theater_ratio, 16, 0.55).
narrative_ontology:measurement(olympic_tr_t25, olympic_host_infrastructure_debt, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(olympic_be_t0, olympic_host_infrastructure_debt, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(olympic_be_t4, olympic_host_infrastructure_debt, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(olympic_be_t8, olympic_host_infrastructure_debt, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(olympic_be_t16, olympic_host_infrastructure_debt, base_extractiveness, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_host_infrastructure_debt, global_infrastructure).
narrative_ontology:affects_constraint(olympic_host_infrastructure_debt, urban_gentrification_displacement).
narrative_ontology:affects_constraint(olympic_host_infrastructure_debt, public_debt_service_crowding).
narrative_ontology:affects_constraint(olympic_host_infrastructure_debt, infrastructure_maintenance_burden).

% DUAL FORMULATION NOTE:
% Olympic host infrastructure debt is upstream of several constraint families: gentrification and displacement mechanisms (separate ε reflecting housing dynamics), public debt crowding effects on education/healthcare funding (separate ε reflecting fiscal opportunity costs), and maintenance burden on degraded infrastructure (separate ε reflecting post-event facility utilization). All three downstream constraints share common originating mechanism but have distinct structural properties and measurement dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(olympic_host_infrastructure_debt, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
