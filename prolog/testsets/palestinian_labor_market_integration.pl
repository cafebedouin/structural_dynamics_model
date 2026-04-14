% ============================================================================
% CONSTRAINT STORY: palestinian_labor_market_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_palestinian_labor_market_integration, []).

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
 *   constraint_id: palestinian_labor_market_integration
 *   human_readable: Palestinian Labor Market Integration Constraint
 *   domain: economic_policy/labor_markets/geopolitical
 *
 * SUMMARY:
 *   The Palestinian labor market integration constraint represents a
 *   structural lock binding Palestinian economic development to Israeli
 *   employment dominance. The system coordinates labor supply and regional
 *   capital flows while simultaneously extracting concessions in security,
 *   political autonomy, and Palestinian state development. Since the early
 *   1990s, Palestinian workers' dependency on Israeli employment (peaking at
 *   ~130,000 in 2000, stabilized at ~60,000-80,000 post-2008) has been
 *   embedded within a permit system that functions simultaneously as a labor
 *   market mechanism and a security apparatus. This creates a tangled rope:
 *   genuine economic coordination exists (Palestinian workers gain income,
 *   Israeli employers access labor, regional supply chains integrate) but the
 *   mechanism is weaponized to suppress Palestinian wage levels, capital
 *   accumulation, and state capacity. The constraint's extractiveness has
 *   increased over the 30-year measurement interval as Palestinian domestic
 *   employment options have contracted and remittance dependency has
 *   deepened.
 *
 * KEY AGENTS:
 *   - Palestinian Workers: Primary victim (powerless/trapped) — earn 20-40% below equivalent Israeli wages, lack employment security, face permit system dependency
 *   - Palestinian Economy: Secondary victim (moderate/constrained) — constrained by capital flight, limited market access, wage suppression; benefits from remittances and supply chain integration
 *   - Israeli Employers: Primary beneficiary (institutional/arbitrage) — access to below-market labor, flexibility to adjust employment, exit options via substitution
 *   - Israeli Security Apparatus: Primary beneficiary (institutional/arbitrage) — uses permit system for surveillance and control, maintains geopolitical leverage
 *   - Palestinian Authority & Security Services: Organized victim (organized/constrained) — self-administers suppression mechanism, receives security subsidy, lacks fiscal autonomy
 *   - International Development Institutions: Secondary beneficiary (institutional/arbitrage) — maintain programs in theater of development while structural barriers remain non-negotiable
 *   - Analytical Observer: Sees tangled rope structure — coordination + extraction + weaponization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(palestinian_labor_market_integration, 0.58).
domain_priors:suppression_score(palestinian_labor_market_integration, 0.72).
domain_priors:theater_ratio(palestinian_labor_market_integration, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(palestinian_labor_market_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(palestinian_labor_market_integration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(palestinian_labor_market_integration, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(palestinian_labor_market_integration, tangled_rope).
narrative_ontology:human_readable(palestinian_labor_market_integration, "Palestinian Labor Market Integration Constraint").
narrative_ontology:topic_domain(palestinian_labor_market_integration, "economic_policy/labor_markets/geopolitical").

domain_priors:requires_active_enforcement(palestinian_labor_market_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(palestinian_labor_market_integration, israeli_employers).
narrative_ontology:constraint_beneficiary(palestinian_labor_market_integration, palestinian_security_contractors).
narrative_ontology:constraint_beneficiary(palestinian_labor_market_integration, settlement_expansion_economy).
narrative_ontology:constraint_victim(palestinian_labor_market_integration, palestinian_workers).
narrative_ontology:constraint_victim(palestinian_labor_market_integration, palestinian_economy).
narrative_ontology:constraint_victim(palestinian_labor_market_integration, palestinian_state_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN WORKER (SNARE) — Trapped by economic dependency with no viable alternatives. Lacks resources to develop domestic employment, faces movement restrictions, and has minimal bargaining power. Suppression operates through permit systems, checkpoint delays, and economic coercion. Experiences maximum extraction: wages below Israeli standards, zero employment security, no social protections.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN ECONOMY (TANGLED ROPE) — Genuinely constrained by lack of capital, infrastructure, and market access, yet benefits from labor remittances and integration into regional supply chains. The constraint coordinates some economic flows while extracting through wage suppression and capital flight. Development is possible but at high cost — requires breaking political and security barriers, not just economic ones.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI EMPLOYERS & SECURITY APPARATUS (ROPE) — Experiences the constraint as a pure coordination mechanism: access to labor, control of movement, economic leverage for security objectives. Net beneficiaries with exit options (can substitute capital, automation, or other labor sources). The constraint solves the coordination problem of maintaining both economic integration and security dominance.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PALESTINIAN AUTHORITY & SECURITY SERVICES (TANGLED ROPE) — Organized agents facing structural constraint. Genuine coordination function: employment absorption, revenue collection, security coordination. But embedded extraction: dependence on Israeli security subsidy, limited fiscal autonomy, use of labor data for surveillance. Suppression is self-administered (permit denial, curfews enforced by PA) — coordination mechanism is weaponized by both sides.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT INSTITUTIONS (PITON) — Maintain a theater of labor-market development policy (skill training, microfinance, export promotion) that operates independently of the structural constraint. Theater ratio is high: actual development capacity is minimal because permits, capital flight, and market access barriers are non-negotiable. The institutions have arbitrage options (exit to other regions) and see their programs as permanent, not recognizing the permitting system as the actual constraint. Piton classification: degraded institutional role maintained through donor funding logic.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees a genuine coordination function (economic integration enabling survival) layered with systematic extraction (suppressed wages, blocked development, permit dependency). The constraint exhibits all the hallmarks of tangled rope: beneficiaries (Israeli employers), victims (Palestinian workers), active enforcement (permit system, checkpoint regime, curfews), and a real coordination mechanism (labor supply, remittance flows, regional supply chains) that is weaponized to extract political and economic concessions.
constraint_indexing:constraint_classification(palestinian_labor_market_integration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(palestinian_labor_market_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(palestinian_labor_market_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(palestinian_labor_market_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(palestinian_labor_market_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(palestinian_labor_market_integration, TR),
    TR >= 0.70.

:- end_tests(palestinian_labor_market_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through suppressed wages (~25-40% below Israeli levels for equivalent work), restricted market access that prevents Palestinian domestic capital accumulation, and political leverage extracted via labor-dependency-mediated security cooperation. The value is not higher (0.72) because some genuine economic benefit flows to Palestinian workers (income when alternatives are absent) and to the Palestinian economy (remittances represent 10-15% of Palestinian GDP). The extraction is real but not absolute. Suppression (0.72): High. Multiple suppression mechanisms operate in parallel: (a) permit system creates formal barriers to employment and movement, (b) checkpoint delays impose transaction costs on commuting, (c) curfews and military operations create periodic unemployment shocks, (d) Palestinian economy lacks capital and market access, creating no viable alternatives. The suppression is both external (imposed by Israeli authorities) and internalized (Palestinian Authority cooperates in administration). Theater ratio (0.65): Moderate-high. International development programs frame Palestinian economic development as a technical problem (skill training, microfinance, entrepreneurship support) while the structural barriers (permits, capital controls, market access) are non-negotiable. The programs deliver some real value (skill acquisition, liquidity infusion) but operate within theater of development that obscures the geopolitical constraint. Theater has increased over time as development rhetoric has expanded while actual Palestinian employment options have contracted.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal on this constraint. Israeli employers and the security apparatus genuinely perceive coordination (Rope) — the permit system solves real problems of labor matching and security. The Palestinian worker perceives pure extraction (Snare) — they are trapped with no alternatives, bearing all costs. The Palestinian economy perceives mixed coordination-extraction (Tangled Rope) — they benefit from wage income and integration but are locked into a subordinate position. The Palestinian Authority perceives themselves as administering coordination (Rope) but the analyst sees them as collaborating in tangled rope. International development institutions perceive a technical problem (Rope or Scaffold) because they work within theater of development that brackets the permit system as non-negotiable. The analytical observer sees the full tangled rope: the coordination mechanisms are real and the extraction is real, they are structurally interwoven, and the whole system is maintained through active enforcement and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position. Israeli employers and security apparatus are beneficiaries with arbitrage options (d ≈ 0.15) — they experience low or negative extraction because the system benefits them directly. Palestinian workers are trapped victims (d ≈ 0.95) — they experience maximum extraction because they have no exit, no bargaining power, and bear all suppression costs. The Palestinian economy is a constrained victim (d ≈ 0.75) — it faces high barriers but some remittance benefits and supply chain integration provide partial mitigation. The Palestinian Authority is an organized but dependent agent (d ≈ 0.55) — they have agency in administering permits but lack autonomy in setting policy; they are both victim and collaborator. International development institutions have arbitrage options and institutional power (d ≈ 0.20) — they can reposition their programs elsewhere, so experienced extraction is low despite working within a high-extraction constraint. The perspectival gap reflects these d values: beneficiaries see rope (low d → low χ), trapped agents see snare (high d → high χ), organized agents see tangled rope (moderate d → moderate χ), and analytical observers see the full structure as tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by disaggregating the perspectives: beneficiaries and trapped agents cannot perceive the same constraint. The 'is this coordination or extraction?' question has different answers depending on d. Beneficiaries truthfully perceive coordination. Trapped agents truthfully perceive extraction. The analytical observer sees both: the constraint is tangled rope because it genuinely coordinates labor supply AND genuinely extracts through wage suppression and development blockade. The mandatrophy dissolves when the framework separates the experiences: all perspectives are correct within their structural position. The false choice ('it must be either rope or snare') is a mislabeling of a single type; tangled rope correctly captures the coexistence of genuine coordination and asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_market_exit_threshold,
    'At what wage premium would Palestinian workers perceive sufficient exit options to reclassify from trapped to constrained?',
    'Comparative wage analysis with internal Palestinian employment; threshold identification where net migration reverses; survey data on worker preferences if alternatives existed',
    'If threshold is low (~15% wage premium to domestic work): trapped classification is firm, systemic change required. If threshold is high (>50%): exit is theoretically possible, permitting system is primary barrier rather than economic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_exit_threshold, empirical, 'Wage threshold for trapped-to-constrained reclassification').

omega_variable(
    permit_system_enforcement_discretion,
    'What proportion of permit denials reflects security rationale versus labor market control?',
    'Archival analysis of permit decisions and stated rationales; correlation between security incidents and denial rates; cross-border employment data during security escalations vs calm periods',
    'If primarily security: suppression is legitimate geopolitical constraint (partial mountain signature). If primarily labor control: suppression is extractive mechanism (confirms snare classification for trapped perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permit_system_enforcement_discretion, conceptual, 'Proportion of permit denials motivated by labor control vs security').

omega_variable(
    palestinian_domestic_employment_substitutability,
    'Could Palestinian domestic employment absorb workers currently dependent on Israeli permits if capital, market access, and security barriers were removed?',
    'Sectoral analysis of Palestinian economy absorptive capacity; comparison with similarly-sized economies; counterfactual modeling with removal of capital and access constraints',
    'If substitutable: the constraint''s extractiveness derives primarily from geopolitical barriers (classification could shift to mountain or scaffold depending on timeline). If not substitutable: Palestinian labor integration is structurally necessary (confirms tangled_rope for economic perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_domestic_employment_substitutability, empirical, 'Whether Palestinian domestic economy could substitute for Israeli employment').

omega_variable(
    remittance_dependency_trajectory,
    'Is remittance dependency increasing or decreasing? Does this reflect constraint strengthening or gradual exit pathway?',
    'Time-series analysis of remittance volumes, wages, employment numbers, and Palestinian GDP; identification of inflection points correlating with policy changes or economic shifts',
    'If increasing: constraint is strengthening (higher extractiveness), Palestinian economy is being locked in. If decreasing: exit pathways are opening (reclassify toward scaffold with sunset). If stable: equilibrium state of tangled rope is maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remittance_dependency_trajectory, empirical, 'Remittance dependency trajectory and interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(palestinian_labor_market_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plmi_tr_t0, palestinian_labor_market_integration, theater_ratio, 0, 0.45).
narrative_ontology:measurement(plmi_tr_t15, palestinian_labor_market_integration, theater_ratio, 15, 0.58).
narrative_ontology:measurement(plmi_tr_t30, palestinian_labor_market_integration, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(plmi_be_t0, palestinian_labor_market_integration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(plmi_be_t15, palestinian_labor_market_integration, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(plmi_be_t30, palestinian_labor_market_integration, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(palestinian_labor_market_integration, resource_allocation).
narrative_ontology:affects_constraint(palestinian_labor_market_integration, palestinian_state_capacity_development).
narrative_ontology:affects_constraint(palestinian_labor_market_integration, israeli_settlement_expansion_economy).
narrative_ontology:affects_constraint(palestinian_labor_market_integration, regional_capital_mobility).

% DUAL FORMULATION NOTE:
% Palestinian labor market integration decomposes into at least three structurally distinct constraints: (1) wage suppression mechanism (ε≈0.65, snare from worker perspective), (2) Palestinian Authority autonomy deficit (ε≈0.55, tangled rope), (3) Palestinian domestic employment development (ε≈0.48, tangled rope with sunset potential if capital barriers removed). The shared measurement interval (30 years) captures how policy decisions in one domain (settlement expansion) constrain outcomes in another (labor market options).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(palestinian_labor_market_integration, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
