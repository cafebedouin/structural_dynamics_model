% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_decentralized_social_provision
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_decentralized_social_provision, []).

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
 *   constraint_id: sotu_1991_bush_decentralized_social_provision
 *   human_readable: Devolution of Social Service Provision from Federal Bureaucracy to Individuals, Families, and Local Communities
 *   domain: social_policy/welfare_administration
 *
 * SUMMARY:
 *   The 1991 Bush administration proposal for devolution of social service
 *   provision from federal bureaucracy to individuals, families, and local
 *   communities represents a structural constraint that reorganizes
 *   decision-making power and resource allocation across scales. The rhetoric
 *   frames devolution as increasing autonomy and choice; the actual mechanism
 *   privileges agents with existing resources (affluent localities, private
 *   providers, organized professionals) to design and provision services,
 *   while shifting costs onto agents lacking either resources or scale to
 *   build alternatives (low-income families, disabled populations in sparse
 *   areas, rural communities). This constraint exhibits characteristic
 *   tangled_rope properties: it contains genuine coordination benefits
 *   (communities can tailor services to local needs, reduce bureaucratic
 *   overhead) alongside systematic extraction (costs of service uniformity
 *   removal fall disproportionately on those who lose federal guarantees).
 *   The constraint's theater_ratio (0.55) reflects significant performative
 *   content—'community empowerment' and 'individual choice' rhetoric obscure
 *   the material reallocation of resources and risk. Over the 15-year
 *   interval, extractiveness increases from 0.32 to 0.62 as devolution's
 *   cumulative effects become visible: geographic inequality in service
 *   provision widens, federal enforcement capacity atrophies, and
 *   community-based alternatives fail to scale in low-resource regions.
 *
 * KEY AGENTS:
 *   - Federal Welfare Bureaucracy: Institutional actor (institutional/constrained) — loses operational function but maintains shadow coordination role through grant distribution and oversight theater
 *   - State Governments: Institutional actors (organized/constrained) — gain design autonomy but receive underfunded mandates; face choice between raising state taxes or cutting services
 *   - Affluent Localities: Regional institutional actors (institutional/arbitrage) — primary beneficiaries; tax base captures service provision contracts and market pricing
 *   - Private Service Providers: Market actors (powerful/arbitrage) — gain service contracts; can cherry-pick profitable service segments and exit unprofitable ones
 *   - Low-Income Rural Populations: Powerless agents (powerless/trapped) — lose federal service guarantees; face service deserts and cost-shifting to families
 *   - Disabled Persons Dependent on Services: Powerless agents (powerless/constrained to trapped) — services become discretionary; support costs shift to unpaid family labor
 *   - Community Organizing Movements: Organized agents (organized/mobile) — build alternative provision structures (mutual aid, cooperatives); see devolution as opportunity for autonomy
 *   - Working-Class Families: Moderate agents (moderate/constrained) — experience choice rhetoric alongside constrained actual choices (market prices exceed wages; coordination burden shifts to unpaid time)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_decentralized_social_provision, 0.58).
domain_priors:suppression_score(sotu_1991_bush_decentralized_social_provision, 0.62).
domain_priors:theater_ratio(sotu_1991_bush_decentralized_social_provision, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_decentralized_social_provision, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1991_bush_decentralized_social_provision, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1991_bush_decentralized_social_provision, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_decentralized_social_provision, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_decentralized_social_provision, "Devolution of Social Service Provision from Federal Bureaucracy to Individuals, Families, and Local Communities").
narrative_ontology:topic_domain(sotu_1991_bush_decentralized_social_provision, "social_policy/welfare_administration").

domain_priors:requires_active_enforcement(sotu_1991_bush_decentralized_social_provision).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_decentralized_social_provision, affluent_localities).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_decentralized_social_provision, market_providers).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_decentralized_social_provision, individual_autonomy_advocates).
narrative_ontology:constraint_victim(sotu_1991_bush_decentralized_social_provision, low_income_rural_populations).
narrative_ontology:constraint_victim(sotu_1991_bush_decentralized_social_provision, disabled_persons_dependent_services).
narrative_ontology:constraint_victim(sotu_1991_bush_decentralized_social_provision, service_uniformity_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL DISABLED POPULATION (SNARE) — Trapped in regions with sparse private providers and underfunded local programs. Lost federal guarantee of uniform service access. No mobility to exit—services are immobile, populations are place-bound. Maximum extraction: guaranteed service becomes discretionary; disability support cost shifts to family (unpaid labor extraction). Cannot organize alternative provision; cannot migrate affordably.
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WORKING-CLASS FAMILY (TANGLED ROPE) — Gains rhetorical autonomy and choice framing, but faces constrained choices: local childcare market prices exceed wages; disability care coordination becomes unpaid family labor. Some genuine coordination benefit (participation in local decision-making) mixed with extraction (costs of service gaps, time poverty from coordination burden, reduced earning capacity due to care demands). Modestly constrained exit—can move to better-resourced locality but at high cost.
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AFFLUENT LOCALITIES & PRIVATE PROVIDERS (ROPE) — Primary beneficiaries. Gain market access, tax base capture, and service provision contracts. High exit capacity through market arbitrage—can provision services to profitable segments and exit unprofitable ones. Net positive extraction inflow: they benefit from devolution rhetoric while capturing resources. Coordinate provision with themselves; experience minimal coercion. Classical rope: coordination mechanism (market-based service matching) with asymmetric benefit flow.
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY ORGANIZING & MUTUAL AID (SCAFFOLD) — Organized agents building parallel provision structures (food banks, childcare cooperatives, disability justice networks, community health clinics). See devolution as opportunity: 'Do not ask federal bureaucracy, build locally.' Low extraction experienced because they have agency and exit pathways (they can exit federal dependency by building alternatives). Sunset logic: as community structures mature and generate sufficient coverage, dependence on either federal or market provision declines. Extraction diminishes as alternatives scale.
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL WELFARE BUREAUCRACY (PITON) — Formally transferred responsibility; maintains shadow coordination role (distributing block grants, enforcing minimal standards, processing data). Original function (direct service provision) atrophied; institution persists through grant administration theater. Sees itself as degraded—maintaining oversight rituals while losing operational legitimacy. Suppression is not applied by this actor but required to maintain it (Congress maintains funding through performative authorization). Theater ratio high: grant reporting, federal compliance audits, and oversight become proxies for actual service verification that has shifted to localities.
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE GOVERNMENTS (TANGLED ROPE) — Mixed experience. Gain autonomy over service design and resource allocation, enabling coordination benefits (match services to local demographics). But receive underfunded mandates: federal block grants often insufficient to cover assumed services; face pressure to fund from state tax base or cut services. Constrained exit—cannot refuse federal requirements but also cannot demand sufficient funding. Coordination function (designing state-appropriate services) mixed with extraction (federal unfunded mandates, upward tax pressure).
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, subsidiarity principle may appear as natural-law organizational imperative: 'Decisions should be made at the lowest competent level; centralized bureaucracies are inherently inefficient.' This perspective risks naturalizing what is actually a policy choice with distributional consequences. The engine will detect this as a false summit—subsidiarity is not a law of nature but a principle that benefits certain agents (those with resources to provision locally) at cost to others (those dependent on redistributive guarantees).
constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_decentralized_social_provision_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_decentralized_social_provision, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1991_bush_decentralized_social_provision, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_decentralized_social_provision, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_decentralized_social_provision_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over interval. At devolution's onset (t=0, ε=0.32), extraction appears minimal—communities enthusiastically embrace autonomy, and some efficiency gains materialize. Over 5-10 years, service inequality begins visibly widening (t=5, ε=0.48; t=10, ε=0.58) as geographic wealth differences amplify under devolved provision. By t=15 (ε=0.62), the cumulative effect is clear: agents without resources to build parallel provision structures face systematically reduced access, while agents with resources capture service provision and cost-shifting benefits. This temporal trajectory is diagnostic of extraction mechanisms that become visible only after initial period of organizational disruption and adaptation. Suppression (0.62): High. Federal enforcement capacity atrophies; local capacity to enforce uniform access standards is limited by funding constraints and technical expertise gaps. Rural populations and disabled service-dependent persons face high barriers to alternative provision (geographic barriers to market services, income barriers to market pricing, organizing barriers to community alternatives). Theater ratio (0.55): Moderate-high. Devolution rhetoric emphasizes 'community empowerment,' 'individual choice,' and 'subsidiarity'—framing that masks resource concentration. Grant administration, federal oversight rituals, and state planning processes become proxy activities replacing actual service verification. Community-based provision claims often exceed actual delivery capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Affluent localities and private providers see rope (coordination without significant coercion). Working-class families see tangled_rope (genuine autonomy mixed with extraction). Rural disabled populations see snare (maximum extraction with no exit). Federal bureaucracy sees piton (degraded ritual without function). State administrators see tangled_rope with asymmetric mandates. Community organizers see scaffold (temporary constraint being solved by alternatives). The analytical observer at civilizational distance risks mountain classification (subsidiarity as natural organizing principle) until the false summit detector flags the benefits flowing to specific agents. The perspectival gap reveals that devolution is not a neutral reorganization but a structural constraint that benefits agents with existing resources to provision locally and costs those dependent on redistributive guarantees. The constraint is 'natural' only from the perspective of those who benefit from naturalizing their advantages.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position in the provision system. Affluent localities and private providers have low d (high beneficiary status, arbitrage exit options)—they experience the constraint as enabling, net-positive extraction flow toward them. Low-income rural populations have high d (victim status, trapped exit)—they experience maximum extraction, losing guarantees without alternative provision capacity. Working-class families have moderate d (mixed victim/beneficiary status, constrained exit)—they gain autonomy rhetoric while facing constrained actual choices and unpaid labor extraction. State administrators have intermediate d (forced both to benefit (design autonomy) and to bear costs (underfunded mandates))—they experience tangled rope. Federal bureaucracy has low d from institutional perspective despite atrophied function—they maintain arbitrage position through grant distribution and can exit operational responsibility. Community organizers have mobile exit options despite victim-adjacent position—they can build alternatives, lowering their experienced d relative to trapped populations. The engine's sigmoid f(d) transformation converts these structural positions to effective extraction multipliers chi, enabling cross-perspective comparison.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination benefits (local tailoring, reduced bureaucratic overhead) from extraction mechanisms (resource concentration, cost-shifting, service uniformity removal). Tangled_rope classification captures both: the constraint has a real coordination function (enabling communities to design appropriate services) alongside systematic extraction (privileging agents with resources, harming agents dependent on guarantees). The extraction increases over time as geographic inequality amplifies—this temporal signature (rising extractiveness, rising theater ratio) distinguishes tangled_rope from pure rope (which would show stable metrics). The snare perspective (rural disabled populations) is not secondary—it represents the structural reality for agents without resources to exit. The rope perspective (affluent localities) is also not primary—it represents the beneficiary's subjective experience, not the constraint's structural nature. The true type is tangled_rope, resolvable through temporal data showing extraction accumulation despite coordination benefits. The scaffold perspective's sunset logic is crucial: if community-based provision can scale, the constraint's extraction mechanism weakens as alternatives mature—this is the pathway from tangled_rope toward rope. If community provision fails to scale (likely in low-resource regions), the constraint's extraction mechanism hardens and approaches snare classification. The measurement interval should extend beyond t=15 to track which pathway dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_resource_endowment_asymmetry,
    'How much of the observed service inequality derives from devolution itself versus pre-existing geographic wealth disparities?',
    'Comparative analysis of service uniformity before/after devolution; decomposition of inequality growth into devolution effect vs baseline wealth effect',
    'If devolution effect > 60%: constraint is genuinely extractive (concentrates inequality that wasn''t before). If devolution effect < 30%: constraint reveals rather than creates inequality—true effect is wealth-driven. If 30-60%: constraint amplifies pre-existing asymmetries (moderate extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_resource_endowment_asymmetry, empirical, 'Devolution vs. pre-existing geographic inequality decomposition').

omega_variable(
    community_capacity_heterogeneity,
    'Can low-resource communities actually build effective provision structures at scale, or is ''community-based'' provision a cover story for removing guarantees?',
    'Time-series analysis of community provision success rates in low-income regions; comparison with devolved and federated service models in other nations',
    'If community capacity exists: scaffold perspective is structural (sunset is real). If capacity is absent: community provision rhetoric is theatrical cover for pure extraction (snare classification dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_capacity_heterogeneity, empirical, 'Whether community-based provision can scale in low-resource areas').

omega_variable(
    choice_rhetoric_vs_actual_agency,
    'Does devolution increase actual decision-making agency for service users, or does it substitute federal bureaucratic constraint with market/local political constraint?',
    'User agency surveys pre/post-devolution; analysis of choice expansion vs. substitution to alternative constraints (e.g., childcare choice replaces welfare eligibility constraint with market price constraint)',
    'If actual agency increases: rope or scaffold classification. If constraints are merely substituted: snare or tangled_rope classification. If agency appears rhetorical: piton classification (performative autonomy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(choice_rhetoric_vs_actual_agency, conceptual, 'Whether devolution increases actual decision agency or substitutes constraint mechanisms').

omega_variable(
    cross_generational_outcome_divergence,
    'Do children in high-resource localities experience measurably better developmental outcomes than those in low-resource localities post-devolution, and does this gap grow over time?',
    'Longitudinal educational attainment, health, and earnings data by locality resource level; trend analysis over 10+ years post-devolution',
    'If outcomes diverge: constraint is extractive (privileges birth lottery over universal guarantee). If outcomes converge: constraint enables efficiency gains that improve low-resource provision despite formal devolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_generational_outcome_divergence, empirical, 'Cross-generational outcome divergence by geographic resource level').

omega_variable(
    market_provision_coverage_gap,
    'What percentage of low-income demand for social services (disability care, childcare for non-working parents, elder care) cannot be met by market provision at any price point?',
    'Market capacity analysis by service type and geography; identification of services structurally unprofitable for private provision',
    'If coverage gap > 40%: market-based devolution cannot replace federal provision for high-need populations (snare classification confirmed). If gap < 20%: market provision is viable foundation (rope or tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_provision_coverage_gap, empirical, 'Market provision coverage gap for low-income social service demand').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_decentralized_social_provision, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devol_tr_t0, sotu_1991_bush_decentralized_social_provision, theater_ratio, 0, 0.35).
narrative_ontology:measurement(devol_tr_t5, sotu_1991_bush_decentralized_social_provision, theater_ratio, 5, 0.42).
narrative_ontology:measurement(devol_tr_t10, sotu_1991_bush_decentralized_social_provision, theater_ratio, 10, 0.52).
narrative_ontology:measurement(devol_tr_t15, sotu_1991_bush_decentralized_social_provision, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(devol_be_t0, sotu_1991_bush_decentralized_social_provision, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(devol_be_t5, sotu_1991_bush_decentralized_social_provision, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(devol_be_t10, sotu_1991_bush_decentralized_social_provision, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(devol_be_t15, sotu_1991_bush_decentralized_social_provision, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_decentralized_social_provision, resource_allocation).
narrative_ontology:affects_constraint(sotu_1991_bush_decentralized_social_provision, healthcare_access_inequality).
narrative_ontology:affects_constraint(sotu_1991_bush_decentralized_social_provision, educational_resource_disparities).
narrative_ontology:affects_constraint(sotu_1991_bush_decentralized_social_provision, disability_service_provision_fragmentation).

% DUAL FORMULATION NOTE:
% Devolution of social provision is downstream of subsidiarity principle and upstream of specific service fragmentation constraints (healthcare access, educational funding, disability support). The three downstream constraints each exhibit different extractiveness profiles depending on local implementation; the devolution story captures the structural mechanism that enables and rewards service inequality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1991_bush_decentralized_social_provision, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
