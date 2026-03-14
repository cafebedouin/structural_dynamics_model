% ============================================================================
% CONSTRAINT STORY: national_sports_infrastructure_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_national_sports_infrastructure_debt, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: national_sports_infrastructure_debt
 *   human_readable: National Sports Infrastructure Debt Constraint
 *   domain: economic/political/urban_development
 *
 * SUMMARY:
 *   National sports infrastructure debt represents a structural extraction
 *   mechanism operating between professional sports franchises and municipal
 *   taxpayers, mediated through public finance instruments. The constraint
 *   exhibits hybrid coordination-extraction characteristics: municipalities
 *   coordinate regional identity and economic development narratives through
 *   stadium investment, while franchises extract subsidy value through
 *   credible relocation threats. The extraction has intensified over the
 *   20-year interval as stadiums have become more capital-intensive (0.35 →
 *   0.58 extractiveness) and as performative economic justification has
 *   replaced empirical analysis (0.48 → 0.64 theater ratio). The constraint
 *   demonstrates all six DR types from different observation points,
 *   revealing how indexical perspective determines what appears to be an
 *   immutable feature of competitive urbanization versus a contingent
 *   institutional arrangement extracting public resources for private
 *   benefit.
 *
 * KEY AGENTS:
 *   - Municipal Taxpayers: Primary victims (powerless/trapped) — bear full debt servicing cost through property taxes; no practical relocation or voting alternative
 *   - Professional Sports Franchises: Primary beneficiaries (institutional/arbitrage) — receive stadium infrastructure, operating subsidies, and tax exemptions; exit via relocation threat ensures favorable terms
 *   - Public Service Agencies: Secondary victims (moderate/constrained) — schools and transit compete for diverted tax revenue; benefit from stadium-adjacent infrastructure development but pay large net cost
 *   - Construction Contractors: Secondary beneficiary (powerful/mobile) — capture design and construction contracts; can exit through market competition but benefit from sustained debt-financed projects
 *   - Corporate Sponsors: Tertiary beneficiary (institutional/arbitrage) — media rights and advertising revenue; benefit from league prestige and tax-advantaged facility access
 *   - Municipal Fiscal Reform Coalition: Organized challenger (organized/constrained) — ballot initiatives, charter amendments, and state restrictions building exit pathways; constrained by incumbent political resistance
 *   - State Government: Institutional arbiter (powerful/mobile) — can reshape bonding authority and restrict local stadium financing; currently benefits from franchise tax revenue and competitive sports culture
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — risks naturalizing competitive urbanization logic as immutable economic law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(national_sports_infrastructure_debt, 0.58).
domain_priors:suppression_score(national_sports_infrastructure_debt, 0.68).
domain_priors:theater_ratio(national_sports_infrastructure_debt, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(national_sports_infrastructure_debt, extractiveness, 0.58).
narrative_ontology:constraint_metric(national_sports_infrastructure_debt, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(national_sports_infrastructure_debt, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(national_sports_infrastructure_debt, tangled_rope).
narrative_ontology:human_readable(national_sports_infrastructure_debt, "National Sports Infrastructure Debt Constraint").
narrative_ontology:topic_domain(national_sports_infrastructure_debt, "economic/political/urban_development").

domain_priors:requires_active_enforcement(national_sports_infrastructure_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(national_sports_infrastructure_debt, professional_sports_franchises).
narrative_ontology:constraint_beneficiary(national_sports_infrastructure_debt, corporate_sponsors).
narrative_ontology:constraint_beneficiary(national_sports_infrastructure_debt, construction_contractors).
narrative_ontology:constraint_victim(national_sports_infrastructure_debt, municipal_taxpayers).
narrative_ontology:constraint_victim(national_sports_infrastructure_debt, public_transit_services).
narrative_ontology:constraint_victim(national_sports_infrastructure_debt, public_education_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Powerless trapped agents cannot exit municipal jurisdiction without severe life disruption. Sports infrastructure debt is enforced through tax bonds backed by municipal authority. Voters have no practical alternative funding source; debt obligations persist regardless of referendum outcomes. Maximum experienced extraction — taxpayers bear full cost of stadium construction and ongoing debt servicing.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Schools and transit systems are structurally constrained by budget competition. They benefit from stadium-adjacent development (improved infrastructure corridors, regional visibility) but pay extraction cost through diverted tax revenue. Constrained exit — agencies can petition for budget protection but cannot exit revenue-sharing agreements without organizational crisis.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Franchises have high exit capacity — they can relocate to alternative municipalities offering better stadium deals. They experience the constraint as pure coordination: stadium finance is a solved problem enabling their core activity. Net beneficiary with effective arbitrage exit (threat of relocation ensures favorable terms).
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Organized advocates (fiscal watchdog groups, transparency organizations, reform-minded mayors) see stadium debt as a temporary coordination failure with policy sunset. State-level ballot initiatives restricting public stadium funding, charter amendments requiring voter approval, and alternative financing models (public ownership, private-equity partnerships) represent exit pathways. Sunset clause embedded in emerging policy frameworks.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The theater ratio reflects that stadium deals persist through institutional inertia and ritual competitive signaling rather than demonstrated economic benefit. Cost-benefit analyses routinely show net negative return; deals proceed anyway. Piton classification — degraded institution maintained through political theater and perceived necessity despite evidence of failure.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% State actors have genuine policy mobility — they can restrict local stadium bonding authority, require referendum approval, or ban public stadium funding. However, they also benefit from franchise presence (tax revenue, economic development narrative) and coordination function (managing regional sports competition). Mixed experience: can reshape rules but benefit from current extraction regime.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer at civilizational scope risks naturalizing stadium debt as an inherent feature of modern urban competition ('cities must compete for franchises') and sports culture ('major league sports requires capital'). This perspective sees the constraint as arising from unchangeable economic logic and collective action problems. However, structural data contradicts mountain classification — comparable international cities operate major sports without public debt mechanisms. The naturalization is contingent, not inevitable.
constraint_indexing:constraint_classification(national_sports_infrastructure_debt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(national_sports_infrastructure_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(national_sports_infrastructure_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(national_sports_infrastructure_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(national_sports_infrastructure_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(national_sports_infrastructure_debt, TR),
    TR >= 0.70.

:- end_tests(national_sports_infrastructure_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original research group captures stadium benefits (visibility, real estate appreciation, corporate partnerships) while municipalities bear debt costs spanning 20-40 years. Extraction value is not as severe as pure subsidy (which would be 0.72+) because franchises provide genuine entertainment/economic coordination services. However, empirical studies show stadiums generate negative net public returns, meaning extraction exceeds coordination benefit. The 20-year trend (0.35 → 0.58) reflects accumulating debt obligations and declining cost-benefit ratios as stadium complexity increases. Suppression (0.68): High. Municipal taxpayers face multiple barriers to exit: legal bond obligations, referendum-proof governance structures, relocation prohibitively expensive, collective action problem (voters scattered across taxing jurisdictions), political narrative that opposes public stadium funding is anti-sports. However, suppression is not total (0.85+) because reform coalitions have achieved state-level restrictions and some municipalities have successfully rejected stadium deals. Theater ratio (0.64): Moderate-high. Economic impact analyses routinely show negative net returns yet projects proceed; decision-making emphasizes rivalry with other cities and league-mandated facility standards rather than fiscal analysis; media coverage frames stadiums as prosperity symbols rather than debt mechanisms. Theater has increased over interval as accounting methods have become more sophisticated at hiding negative externalities.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates complete variation across all six types despite identical base properties (ε=0.58, suppression=0.68), showing that indexical position determines classification. The beneficiary (franchise) experiences coordination (Rope). The victim (taxpayer) experiences extraction (Snare). The mixed actor (public agency) experiences hybrid (Tangled Rope). The organized reformer experiences temporary failure with exit pathway (Scaffold). The institutional establishment sees degraded ritual (Piton). The analytical observer sees either natural law (false summit) or contingent arrangement (true structure) depending on whether they naturalize competitive urbanization. The gap is the analytical signal: if all perspectives produced the same type, the constraint would be type-invariant (mountain-like). The six-way split reveals contingent institutional arrangements masquerading as necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims flow in opposite directions. Franchises (beneficiaries, institutional/arbitrage) experience low d → low χ → experience constraint as coordination benefit. Taxpayers (victims, powerless/trapped) experience high d → high χ → experience constraint as extraction cost. Public agencies (mixed, moderate/constrained) experience moderate d → moderate χ → experience constraint as trade-off. State government (powerful/mobile, mixed beneficiary/arbiter status) experiences d ≈ 0.48 → can reshape constraint but benefits from status quo. Reform coalition (organized/constrained, victim advocate) experiences d ≈ 0.55 → can win reforms but face incumbent resistance. Spatial scope modulates: local scope (σ=0.8) dampens effective extraction; national scope (σ=1.0) normalizes it. Franchise relocation threat constrains state and local action (raises d for authorities, lowers perceived exit capacity). The directionality chain reveals: franchise benefits are directly proportional to taxpayer extraction, mediated through municipal arbitrage capacity (ability to raise bonds).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Tangled Rope classification is justified: genuine coordination function (regional identity, economic development narrative) exists alongside asymmetric extraction (franchise benefit >> taxpayer benefit). The coordination is not epiphenomenal — removing stadiums would genuinely degrade regional economic narrative and reduce sports access. However, the extraction systematically exceeds the coordination benefit by empirical measures: cost-benefit analyses show net negative public returns. The constraint is NOT Rope (pure coordination) because extraction asymmetry is structural and measurable. It is NOT Snare (pure extraction) from all perspectives because franchises and municipalities do coordinate genuine regional development. The Tangled Rope classification holds across the empirically weighted perspectives (victims + agencies + authorities) despite individual perspectives seeing it as Snare or Rope. The mandatrophy resolution: this is legitimately a hybrid, and the hybrid classification is stable across measurement methodologies (economic returns, employment analysis, tax revenue accounting).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact_measurement_gaming,
    'Are reported economic returns from stadium development systematically overstated through selective impact metrics and displacement of existing economic activity?',
    'Longitudinal comparison of pre-stadium and post-stadium employment and tax revenue in stadium districts versus control neighborhoods; accounting for jobs relocated rather than created; adjustment for counterfactual development absent stadium',
    'If true: stadium debt is pure extraction masked by accounting theater. Extractiveness should be revised upward to 0.68+, moving classification from Tangled Rope toward Snare. If false: genuine coordination benefits justify moderate extraction, supporting Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_impact_measurement_gaming, empirical, 'Whether stadium economic impact metrics are gamed through selective accounting').

omega_variable(
    franchise_relocation_credibility,
    'Is the threat of franchise relocation genuine and binding on municipal decision-making, or a rhetorical device that rarely actualizes?',
    'Historical analysis of relocation threats versus actual relocations; examination of counterfactual municipal financing decisions absent relocation threat; survey of franchise decision-making (do relocation threats actually influence location choice versus post-hoc justification)',
    'If threat is real: arbitrage exit for franchises is credible, justifying low d and institutional benefit framing. If threat is rhetorical: franchises are constrained to existing markets and relocation threat is extortive performance, raising d substantially and reclassifying toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_relocation_credibility, empirical, 'Whether franchise relocation threat is credible versus rhetorical').

omega_variable(
    policy_reform_window_closure,
    'As state-level restrictions on public stadium funding proliferate, does the fiscal reform coalition''s scaffold exit pathway materialize, or does circumvention through alternative bonding mechanisms preserve the constraint?',
    'Tracking of state ballot initiatives and municipal charter amendments restricting public stadium funding; analysis of deals completed before vs after restrictions; identification of novel financing workarounds (sports authorities, enterprise zones, tax increment financing) that preserve extraction while changing legal form',
    'If reforms succeed: scaffold sunset is real, constraint weakens over generational horizon (20-30 years). If circumvented: constraint persists through institutional adaptation, theater ratio increases further (0.64 → 0.78+), and Piton classification becomes more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_reform_window_closure, empirical, 'Whether state fiscal reform creates genuine sunset or is circumvented through legal workarounds').

omega_variable(
    taxpayer_identity_lock_mechanism,
    'Do municipal taxpayers experience sports infrastructure debt as an immutable feature of civic identity (we are a major league city) that prevents them from perceiving policy exit options, or as an economic extraction they would resist if given agency?',
    'Survey analysis of taxpayer framing (is debt described as inevitable cost of major-league status versus imposed financial burden); referendum voting patterns on stadium restrictions; analysis of comparable jurisdictions that rejected stadium debt without losing sports access or civic status',
    'If identity-locked: suppression metric may be overstated (cognitive rather than structural); taxpayers perceive constraint as mountain when it is contingent. If economically rational resistance: suppression is structural, trapped exit is accurate, Snare classification more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxpayer_identity_lock_mechanism, conceptual, 'Whether taxpayer constraint experience is identity-locked or economically rational resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(national_sports_infrastructure_debt, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsid_tr_t0, national_sports_infrastructure_debt, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nsid_tr_t10, national_sports_infrastructure_debt, theater_ratio, 10, 0.58).
narrative_ontology:measurement(nsid_tr_t20, national_sports_infrastructure_debt, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(nsid_be_t0, national_sports_infrastructure_debt, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nsid_be_t10, national_sports_infrastructure_debt, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(nsid_be_t20, national_sports_infrastructure_debt, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(national_sports_infrastructure_debt, resource_allocation).
narrative_ontology:boltzmann_floor_override(national_sports_infrastructure_debt, 0.18).
narrative_ontology:affects_constraint(national_sports_infrastructure_debt, municipal_bond_market_extraction).
narrative_ontology:affects_constraint(national_sports_infrastructure_debt, professional_sports_franchise_arbitrage).

% DUAL FORMULATION NOTE:
% National sports infrastructure debt is upstream of franchise relocation dynamics and municipal fiscal policy constraints. The three constraints form a family: (1) national_sports_infrastructure_debt (ε=0.58, Tangled Rope) — the direct extraction mechanism; (2) municipal_bond_market_extraction (ε=0.72, Snare) — the financial instrument enabling debt capture; (3) professional_sports_franchise_arbitrage (ε=0.40, Rope) — the coordination mechanism (leagues coordinating inter-city competition). Each story tracks different causal mechanisms with distinct ε values per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(national_sports_infrastructure_debt, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
