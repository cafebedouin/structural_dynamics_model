% ============================================================================
% CONSTRAINT STORY: agricultural_contract_grower_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_contract_grower_lockin, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: agricultural_contract_grower_lockin
 *   human_readable: Poultry Contract Grower Lock-In
 *   domain: agricultural_economics/labor_extraction
 *
 * SUMMARY:
 *   The poultry contract-grower system in the United States represents a
 *   structural transformation of agricultural production from independent
 *   farming to dependent labor disguised as entrepreneurship. Over the past
 *   30 years, vertical integration has concentrated control of poultry supply
 *   chains into a handful of large integrators (Tyson, Pilgrim's Pride,
 *   Perdue). Individual growers, presented as independent contractors,
 *   operate under binding contracts that strip them of price-setting power,
 *   market access, and exit capacity. Growers invest $500,000–$1 million in
 *   specialized housing and equipment, financed through loans the integrator
 *   effectively controls through contract decisions. Once indebted, the
 *   grower is locked in: the integrator controls feed prices, flock
 *   placement, payment deductions, and contract renewal. Defection is
 *   economically suicidal. Class-level resistance (organizing, regulation,
 *   antitrust action) is systematically suppressed through contract
 *   termination threats, selective delisting (removing successful organizers
 *   from placement lists), and integrator lobbying that captures state
 *   agricultural agencies. The constraint exhibits strong and intensifying
 *   extraction: grower share of retail poultry value has declined from ~28%
 *   (1970s) to ~8% (2020s), while integrator margins have risen. Theater
 *   ratio (0.45) is moderate because some coordination function is genuine —
 *   the integrator system does achieve economies of scale and consistency.
 *   But the performative content has risen as the justifying narrative
 *   (grower entrepreneurship, risk-sharing, efficiency) has diverged farther
 *   from material reality (locked-in debt, zero risk control, labor
 *   extraction). Accessibility collapse is severe and deepening: debt
 *   consolidation, infrastructure specialization, market concentration, and
 *   integrator supply-control leave growers with no practical exit.
 *   Individual-level suppression is high and rising; organizational-level
 *   suppression has intensified as integrators deploy contract terms and
 *   retaliation against organized growers; class-level suppression operates
 *   through regulatory capture and structural market barriers. The
 *   coercion_grid captures this level-differentiated deepening: individual
 *   accessibility_collapse rises from 0.72 to 0.88 (debt tightens the trap);
 *   organizational suppression rises from 0.68 to 0.80 (integrators harden
 *   anti-organizing tactics); structural stakes_inflation remains low
 *   (system-level, the integrator sector is stable) but organization-level
 *   stakes_inflation rises (individual grower's financial stake in compliance
 *   intensifies as debt grows). Resistance declines at all levels as
 *   suppression hardens and exit becomes more foreclosed.
 *
 * KEY AGENTS:
 *   - Individual Contract Grower: Powerless/trapped (biographical). Operates under integrator contract; debt on housing/equipment forecloses exit; income dominated by integrator pricing. Trapped because no realistic alternative exists once indebted.
 *   - Grower Organizing Coalition: Organized/constrained (generational). Attempts collective bargaining and legislative action; faces contract termination threats and integrator retaliation; some agency but severe suppression.
 *   - Poultry Integrator (e.g., Tyson Foods): Institutional/arbitrage (generational). Controls feed supply, pricing, and market access; benefits from vertical integration; experiences the system as coordination and efficiency.
 *   - State Agricultural Agency: Institutional/constrained (generational). Tasked with regulating contracts and labor conditions; chronically underfunded; captured by integrator lobbying; attempted enforcement but weak capacity.
 *   - Regulatory Reform Coalition: Organized/constrained (generational). Legislative and advocacy groups pushing for antitrust action, contract transparency, pricing regulation; faces integrator lobbying and resource barriers.
 *   - Analytical Observer: Analytical/analytical (civilizational). Views the system as structural extraction with minimal genuine coordination justification; alternatives suppressed through market concentration and regulatory capture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_contract_grower_lockin, 0.78).
domain_priors:suppression_score(agricultural_contract_grower_lockin, 0.72).
domain_priors:theater_ratio(agricultural_contract_grower_lockin, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_contract_grower_lockin, extractiveness, 0.78).
narrative_ontology:constraint_metric(agricultural_contract_grower_lockin, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(agricultural_contract_grower_lockin, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(agricultural_contract_grower_lockin, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(agricultural_contract_grower_lockin, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_contract_grower_lockin, snare).
narrative_ontology:human_readable(agricultural_contract_grower_lockin, "Poultry Contract Grower Lock-In").
narrative_ontology:topic_domain(agricultural_contract_grower_lockin, "agricultural_economics/labor_extraction").

domain_priors:requires_active_enforcement(agricultural_contract_grower_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_contract_grower_lockin, poultry_integrators).
narrative_ontology:constraint_victim(agricultural_contract_grower_lockin, contract_growers).
narrative_ontology:constraint_victim(agricultural_contract_grower_lockin, rural_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(agricultural_contract_grower_lockin, poultry_integrator).
narrative_ontology:constraint_victim(agricultural_contract_grower_lockin, individual_contract_grower).
narrative_ontology:constraint_victim(agricultural_contract_grower_lockin, grower_organizing_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under integrator contract; owns housing and equipment financed through loans; receives income calculated by integrator formula; cannot exit without losing capital investment; faces debt service pressure regardless of product price or personal circumstances.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, individual_contract_grower, payer,
    powerless, biographical, trapped, regional).

% Sets contract terms, controls feed supply and pricing, places flocks with growers, determines payment deductions; captures integrator margin (difference between wholesale poultry price and grower payment); builds supply network by leveraging grower debt and market concentration.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, poultry_integrator, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(agricultural_contract_grower_lockin, poultry_integrator, beneficiary).

% Collective bargaining effort by growers; attempts to negotiate contract terms, pricing floors, and regulatory protections; faces contract termination threats against members; has some political leverage (state legislatures, media attention) but limited structural power against market-concentrated integrators.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, grower_organizing_coalition, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(agricultural_contract_grower_lockin, grower_organizing_coalition, observer).

% Regulatory body tasked with enforcing contract fairness, labor standards, and antitrust rules; chronically underfunded; subject to integrator lobbying; attempts enforcement but lacks capacity; regulations often written in consultation with integrators.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, state_agricultural_agency, agenda_setter,
    institutional, generational, constrained, national).

% Depend on poultry industry employment and grower income; lack economic alternatives as consolidation eliminates independent farms and local processing facilities; suffer environmental damage (waste runoff, air quality) from concentrated operations; have weak voice in contract or regulatory decisions.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, rural_communities, payer,
    powerless, generational, constrained, regional).

% Purchases poultry at low prices enabled by integrator extraction from growers and consolidation; benefits from efficiency but unaware of labor and environmental costs; has no presence in contract negotiations or regulatory decisions.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, consumer_market_for_chicken, excluded,
    powerless, immediate, mobile, national).

% Alternative poultry production model (cooperative marketing, direct sales, niche markets) exists at small scale but is suppressed by integrator market consolidation and lack of access to conventional distribution channels. Have expertise and motivation but face structural barriers to scaling.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, independent_smallholder_producers, excluded,
    moderate, generational, constrained, regional).

% Federal and state subsidies (indirect via crop insurance, commodity programs, infrastructure investment) support integrator viability and efficiency by keeping feed costs low; does not directly participate in contract but enables integrator cost structure.
narrative_ontology:constraint_stakeholder(agricultural_contract_grower_lockin, agricultural_subsidy_system, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(agricultural_contract_grower_lockin, agricultural_subsidy_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(agricultural_contract_grower_lockin, poultry_integrator).
narrative_ontology:fixing_cost_class(agricultural_contract_grower_lockin, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vertical integration achieves real coordination: standardized feed, centralized quality control, economies of scale in processing and distribution, stable supply to retailers, animal health management at scale. The coordination problem solved is matching supply (many small producers) to demand (consolidated retail channels requiring consistent large volumes and quality). Alternatives exist (cooperative marketing, spot markets, direct-to-consumer sales) but at lower efficiency and higher cost.
% TRANSFER_FUNCTION: The arrangement transfers economic value (poultry production surplus) from growers to integrators. Growers receive contracted payment calculated by integrator formula; integrators receive wholesale market price minus grower payment. The integrator margin has grown from ~12–15% of retail price (1980s) to ~25–30% (2020s), while grower share fell from ~28% to ~8%. Debt payments (housing loans, operating credit) transfer wealth from grower families to lenders, primarily controlled by integrators. Risk (price volatility, disease, environmental regulation) is transferred from integrators to growers.
% ABSENT_VOICES: Grower organizing leadership (those who speak out) face contract termination, selective delisting, and economic retaliation — their voices are suppressed, not absent, but de facto excluded from formal negotiations. Independent smallholder producers and cooperative marketing advocates are excluded from consolidation-era policy discussions. Consumers who would pay more for differently-produced poultry (higher-welfare, better-paid labor, environmental stewardship) are excluded from choice because integrators control distribution and pricing. Environmental and labor advocates have limited standing in agricultural policy relative to integrator lobbying.
% DISAPPEARANCE_RATIONALE: If contract grower lock-in disappeared overnight (via antitrust breakup, regulatory reform, or integrator voluntary exit), the poultry system would substantially rearrange. Growers could renegotiate terms or switch integrators; some would exit the industry; new producers might enter (smallholders, cooperatives, alternative producers); vertical integration would likely persist but at weaker terms; prices would shift toward true cost-of-production + fair margin, raising retail poultry prices. Rural economies would face severe adjustment but would gain income distribution to growers and flexibility. The world does not return to pre-consolidation structure but rearranges significantly.
% FOUNDING_PROBLEM: Contract grower systems were developed in the 1950s–1970s to solve the problem of coordinating small independent poultry producers with consolidated retail and processing infrastructure. Retailers demanded large, consistent, quality-controlled volumes; independent producers could not meet these demands individually. Integrators solved this by controlling production standards, feed supply, and genetics, creating the vertical integration model. The founding problem was genuine: coordination of fragmented production.
% FOUNDING_PROBLEM_CORROBORATION: The founding coordination problem is attested by integrator narratives and by historical economic analysis (e.g., USDA Poultry Industry Documentation from 1970s–1980s). However, the problem's persistence is contested: agricultural economists and alternative-agriculture advocates (e.g., Slow Food, cooperative agriculture movements, EU small-producer policies) argue that cooperative marketing and direct-to-consumer sales have now solved the original coordination problem at smaller scales; vertical integration is maintained because it is profitable for integrators, not because alternatives are unviable. EU regulatory frameworks (cooperative producer protections, limits on integrator market share) suggest alternatives are possible. No independent party outside the integrator industry corroborates that vertical integration is still the only viable solution.
narrative_ontology:disappearance_verdict(agricultural_contract_grower_lockin, world_rearranges).
narrative_ontology:founding_problem_status(agricultural_contract_grower_lockin, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN GROWER (SNARE) — Powerless individual grower with no exit capacity. Debt on specialized housing and equipment forecloses alternatives; integrator controls feed, pricing, and flock placement; market concentration makes defection suicidal. Experiences maximum extraction: capital trapped, income dominated, exit costs prohibitive.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZING GROWERS / COALITION (SNARE) — Organized growers attempting collective bargaining face suppression: integrators threaten contract termination, reorganize supply networks to isolate leaders, deploy economic retaliation against activist growers. Coalition has some agency but meets severe enforcement pressure. Still snare, not tangled rope, because coordination function is absent — the constraint exists to extract, not to solve a collective problem.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POULTRY INTEGRATOR (SNARE FROM BENEFICIARY SEAT) — Institutional beneficiary experiencing this as rope: they see coordination (standardization, supply chain control). But the structure is snare for growers — the integrator's 'coordination' benefit is isomorphic with grower extraction. The beneficiary perspective is inverted: low d (beneficiary seat + arbitrage exit) produces negative chi (subsidy), and the classification reverts to rope. The asymmetry is the diagnostic: integrator sees rope, grower sees snare, same structure.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — State or federal agency tasked with agricultural oversight sits between grower and integrator: genuine coordination function (labor standards, environmental rules, contract fairness) exists but is systematically underfunded and captured by integrator lobbying. The agency coordinates rule-making (genuine function) but enforcement is weak (asymmetric extraction from growers who face threats; integrators face minor fines). Active enforcement is attempted but toothless.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VERTICAL INTEGRATION NORM (PITON) — The institutional narrative claims contract grower systems represent efficient coordination and grower entrepreneurship. This framing persists despite evidence that it extracts rent and forecloses exit. The theater_ratio (0.45) is moderate because the performative content is not overwhelming — vertical integration does achieve some coordination benefits. But the gap between the entrepreneurship narrative and the trapped-debt reality is the piton signal: a degraded justification maintained through institutional inertia.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational analytical distance, contract grower lock-in is a structural extraction mechanism with minimal coordination justification. The system extracts labor and capital from dispersed powerless agents and concentrates economic control in integrators. Alternatives exist (independent smallholder production, cooperative marketing) but are suppressed. Accessibility collapse is high because debt and market concentration make exit genuinely unavailable. Resistance is moderate because grower organizing is expensive and faces retaliation.
constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_contract_grower_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_contract_grower_lockin, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_contract_grower_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agricultural_contract_grower_lockin, TR),
    TR >= 0.70.

:- end_tests(agricultural_contract_grower_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and intensifying. The constraint extracts substantial economic value from growers to integrators through multiple mechanisms: (1) Price suppression — grower share of retail value has fallen from ~28% to ~8% over 30 years, with integrators capturing the difference as margin. (2) Risk transfer — integrators shift feed price volatility, disease risk, and market fluctuations onto growers while maintaining price control. (3) Debt leverage — the $500k–$1M debt on specialized housing gives integrators effective control over grower behavior and exit capacity. (4) Information asymmetry — contract terms are complex; growers cannot predict income; pricing formulas contain integrator discretion. The trajectory shows extraction deepening: earlier contracts (1980s–1990s) offered some margin flexibility and informal bargaining room; contemporary contracts (2015–2025) are tighter, include stricter performance clauses, and shift more costs to growers. Base_extractiveness increased from 0.52 (year 0) to 0.78 (year 30) as debt has accumulated, market concentration has deepened, and integrator pricing power has hardened. Suppression (0.72): High. The constraint operates through multiple suppression mechanisms: (1) Economic — contract termination threats remove livelihoods; integrators can delist organizers from placement queues. (2) Legal — restrictive covenants in contracts (non-disparagement clauses, forced arbitration, non-compete provisions) prevent organizing and legal action. (3) Informational — integrators control data on pricing, costs, and contract terms; growers lack transparency. (4) Structural — market concentration (top 4 integrators control ~65% of broiler capacity) eliminates alternatives; geographic isolation of growers reduces organizing capacity. Suppression has risen from 0.58 to 0.72 as integrators hardened anti-organizing tactics, tightened contract language, and consolidated market share. Theater ratio (0.45): Moderate and slightly rising. The system justifies itself through several performative narratives: (1) Grower entrepreneurship — contracts are described as independent business relationships, but growers operate as de facto wage laborers. (2) Risk-sharing — integration is presented as protecting growers from market volatility, but integrators actually control the sources of volatility and shift risk onto growers. (3) Efficiency — vertical integration achieves real economies of scale in feed, processing, and distribution, justifying some coordination premium. The theater has risen from 0.38 to 0.45 because the gap between narrative (independent contractors reaping efficiency benefits) and material reality (debt-locked laborers with zero price control) has widened; the integrators have invested more in the legitimacy narrative as actual extraction has increased, creating defensive theater. Accessibility collapse (0.85): Very high. Growers face severe and compounding barriers to exit: (1) Debt — $500k–$1M invested in specialized poultry housing with no alternative use; refinancing requires integrator credit, giving integrators de facto debt control. (2) Infrastructure specialization — poultry houses cannot be repurposed; resale value is near-zero if integrator refuses placement. (3) Market concentration — only a handful of integrators exist; switching integrators requires selling current contract for pennies on the dollar. (4) Geographic isolation — poultry production is concentrated in specific regions; relocation is not feasible for most growers. (5) Skill trap — growers have invested 20–30 years in poultry expertise; alternative agricultural income is lower. The individual-level accessibility_collapse rose from 0.72 to 0.88 over the interval; the structural-level collapse (the system itself) is consistently high at 0.82–0.89 because the integrator oligopoly and infrastructure specialization are system features, not individual barriers. Resistance (0.35): Moderate but declining. Despite suppression, growers have mounted organizing efforts (National Chicken Council, KAFB, state legislatures), but resistance has weakened because suppression has hardened and exit barriers have deepened. Individual resistance is very low (0.18–0.12) because isolated growers have little leverage; organizational resistance is moderate (0.45–0.38) because coalitions can mount legislative campaigns and media pressure; structural-level resistance is low (0.32–0.28) because reform requires antitrust action or major regulatory overhaul, both of which face integrator lobbying and political gridlock.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme: the integrator sees rope (coordination, efficiency, risk-sharing), while the grower sees snare (extraction, control, exit foreclosure). The analytical observer agrees with the grower: the structure is snare, with coordination benefits as cover. The regulatory agency sees tangled rope — genuine coordination function (food safety, standardization) but weakly enforced asymmetric extraction (growers bear costs of regulation; integrators capture benefits). The piton perspective (vertical integration norm) highlights the gap between the entrepreneurship narrative and the debt-labor reality. The organized growers see snare with some coordinating function (integrators do deliver infrastructure and market access) but recognition that the coordination is purchased through extraction. This is a case where the perspectival gap is the entire diagnostic: the system's legitimacy depends on the integrator and beneficiary perspective being the default, preventing growers' snare perspective from reaching public consciousness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the constraint. For individual growers: d = 0.95 (nearly complete target). They are trapped (no exit option), powerless (no market leverage), and victims of systematic extraction (price suppression, debt leverage, risk transfer). The engine computes high chi from this high d and the base extractiveness. For the integrator: d = 0.05 (nearly complete beneficiary). They are institutional, have arbitrage exit (can shift to other sectors), and are the identified beneficiary collecting extraction rents. The engine computes negative chi (subsidy) for this perspective, explaining the integrator's rope classification. For the organized growers coalition: d = 0.75 (high target, but some agency). They have some market leverage (can organize, publicize, lobby) but face severe suppression and exit barriers. The engine computes high chi but somewhat modulated by the organizational power level and constrained exit options. For the regulatory agency: d = 0.50 (symmetric, under capture). They coordinate (food safety rules) and extract (regulatory costs borne by growers; integrators lobby to shape rules in their favor). The high suppression value and requires_active_enforcement flag reflect that the state must actively defend the system against reform.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outlived function) is UNRESOLVED for this constraint. The mandate of contract grower systems — efficient coordinated poultry production — is still live and structurally important (US poultry production depends on the vertical integration model). However, there is an incipient mandatrophy in the regulatory and political framing: the legislative mandate for agricultural fair practices and the Sherman Act prohibitions on anticompetitive conduct have been superseded by integrator-captured regulatory practice, where rules are written to protect integrators rather than enforce fair competition. This is not mandatrophy of the primary constraint (lock-in itself is still functional for integrators) but of the secondary mandate (regulatory oversight). If antitrust action were taken and contract terms reformed, the primary mandate might genuinely atrophy — integrators might exit vertical integration for spot-market procurement, and the lock-in constraint would disappear. The analytical question is whether reform is possible without destroying coordination benefits. The snare classification asserts that coordination benefits are minimal cover for pure extraction; the piton perspective acknowledges real coordination but degraded justification. The tension between these perspectives is the mandatrophy trigger.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_vs_capital_trap_mechanism,
    'Is the lock-in primarily a debt trap (the grower cannot afford to exit because of borrowed money) or a capital trap (the specialized assets have no alternative use)?',
    'Longitudinal analysis of grower exits: proportion who exit with debt paid vs unpaid; value realization of abandoned infrastructure; alternative asset markets for poultry housing.',
    'If primarily debt: constraint classification remains snare but suppression may be lower (grower could theoretically refinance or restructure). If primarily capital: suppression is higher (assets are truly sunk) and exit is more foreclosed. If both: compounding trap justifies high accessibility_collapse and high suppression values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_vs_capital_trap_mechanism, empirical, 'Whether lock-in is debt-driven or capital-driven').

omega_variable(
    contract_pricing_mechanism_contestation,
    'Are contract grower prices set by transparent formula (costs + fair margin) or by integrator discretion (arbitrary deductions, shifting cost allocation)?',
    'Contract analysis: comparison of stated pricing formulas vs actual payments; audits of cost allocation; comparison across integrators and regions.',
    'If transparent formula: extraction is lower (growers understand the deal) and snare classification weakens toward tangled rope. If arbitrary: extraction is higher and snare is strengthened (growers cannot predict income or negotiate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contract_pricing_mechanism_contestation, empirical, 'Contract pricing transparency and mechanism').

omega_variable(
    regulatory_capture_depth,
    'Is regulatory capture partial (some enforcement attempts, some grower protections enacted but weakly applied) or total (regulations written by integrator lobbyists, no enforcement)?',
    'Regulatory agency staffing and budget over time; frequency of citations and enforcement actions; contract review and dispute resolution activity; integrator participation in rulemaking.',
    'If partial: tangled rope classification of the regulatory agency holds, and there is a structural contradiction (some coordination, some extraction). If total: regulatory agency becomes piton (theater of coordination with no function), and the snare classification deepens across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of regulatory capture by integrators').

omega_variable(
    collective_action_suppression_mechanism,
    'What specific enforcement tactics suppress class-level resistance? Are they economic (contract termination, selective delisting), informational (surveillance, reputation attacks), or legal (restrictive covenants, forced arbitration)?',
    'Documentation of retaliation against organizers; contract analysis for restrictive clauses; interviews with organized growers about suppression mechanisms.',
    'Identifies whether suppression is structural (covenants that prevent organizing) or behavioral (integrator tactics). Structural suppression justifies higher suppression values and stronger snare classification. Behavioral suppression suggests suppression is maintained by actor choice, not system design, opening for intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_suppression_mechanism, empirical, 'Specific mechanisms suppressing class-level resistance').

omega_variable(
    alternative_market_viability,
    'Could independent smallholder production or cooperative marketing actually replace the integrator system at scale, or are network effects and vertical integration genuinely efficiency-superior?',
    'Historical analysis of smallholder poultry markets in regions with strong cooperatives (e.g., certain EU countries, Japan); cost comparison studies; profitability of cooperative models.',
    'If alternatives are viable: accessibility_collapse is partly constructed (alternatives are suppressed, not unavailable), and the classification is strengthened toward snare. If alternatives are efficiency-inferior: the mountain-or-rope perspective gains ground — vertical integration may be a genuine economic fact, not extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_viability, empirical, 'Viability of alternative production models').

omega_variable(
    identity_fusion_in_agricultural_identity,
    'Are growers trapped by material barriers (debt, specialized assets, geographic isolation) or by identity fusion (self-concept as independent farmers captured by the integrator relationship, inability to imagine exit)?',
    'Grower interviews: can growers articulate specific exit barriers, or do they describe exit as unthinkable? Psychological assessment of identity-lock mechanisms.',
    'If material: exit_options should be ''trapped'', and d is high via barriers. If identity-locked: exit_options should be ''identity_locked'', which classifies differently at biographical time (appears as rope rather than mountain). Mixed mechanism implies both values matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_agricultural_identity, conceptual, 'Material versus identity-based dimensions of the lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_contract_grower_lockin, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agri_lock_tr_t0, agricultural_contract_grower_lockin, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(agri_lock_tr_t0, observed).
narrative_ontology:measurement(agri_lock_tr_t10, agricultural_contract_grower_lockin, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(agri_lock_tr_t10, observed).
narrative_ontology:measurement(agri_lock_tr_t20, agricultural_contract_grower_lockin, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(agri_lock_tr_t20, observed).
narrative_ontology:measurement(agri_lock_tr_t30, agricultural_contract_grower_lockin, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(agri_lock_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(agri_lock_be_t0, agricultural_contract_grower_lockin, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(agri_lock_be_t0, observed).
narrative_ontology:measurement(agri_lock_be_t10, agricultural_contract_grower_lockin, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(agri_lock_be_t10, observed).
narrative_ontology:measurement(agri_lock_be_t20, agricultural_contract_grower_lockin, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(agri_lock_be_t20, observed).
narrative_ontology:measurement(agri_lock_be_t30, agricultural_contract_grower_lockin, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(agri_lock_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(agri_lock_su_t0, agricultural_contract_grower_lockin, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(agri_lock_su_t0, observed).
narrative_ontology:measurement(agri_lock_su_t10, agricultural_contract_grower_lockin, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(agri_lock_su_t10, observed).
narrative_ontology:measurement(agri_lock_su_t20, agricultural_contract_grower_lockin, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(agri_lock_su_t20, observed).
narrative_ontology:measurement(agri_lock_su_t30, agricultural_contract_grower_lockin, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(agri_lock_su_t30, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=30
narrative_ontology:measurement(agri_lock_grid_01, agricultural_contract_grower_lockin, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(agri_lock_grid_02, agricultural_contract_grower_lockin, accessibility_collapse(class), 30, 0.78).
narrative_ontology:measurement(agri_lock_grid_03, agricultural_contract_grower_lockin, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(agri_lock_grid_04, agricultural_contract_grower_lockin, accessibility_collapse(individual), 30, 0.88).
narrative_ontology:measurement(agri_lock_grid_05, agricultural_contract_grower_lockin, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(agri_lock_grid_06, agricultural_contract_grower_lockin, accessibility_collapse(organizational), 30, 0.81).
narrative_ontology:measurement(agri_lock_grid_07, agricultural_contract_grower_lockin, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(agri_lock_grid_08, agricultural_contract_grower_lockin, accessibility_collapse(structural), 30, 0.89).
narrative_ontology:measurement(agri_lock_grid_09, agricultural_contract_grower_lockin, resistance(class), 0, 0.42).
narrative_ontology:measurement(agri_lock_grid_10, agricultural_contract_grower_lockin, resistance(class), 30, 0.35).
narrative_ontology:measurement(agri_lock_grid_11, agricultural_contract_grower_lockin, resistance(individual), 0, 0.18).
narrative_ontology:measurement(agri_lock_grid_12, agricultural_contract_grower_lockin, resistance(individual), 30, 0.12).
narrative_ontology:measurement(agri_lock_grid_13, agricultural_contract_grower_lockin, resistance(organizational), 0, 0.45).
narrative_ontology:measurement(agri_lock_grid_14, agricultural_contract_grower_lockin, resistance(organizational), 30, 0.38).
narrative_ontology:measurement(agri_lock_grid_15, agricultural_contract_grower_lockin, resistance(structural), 0, 0.32).
narrative_ontology:measurement(agri_lock_grid_16, agricultural_contract_grower_lockin, resistance(structural), 30, 0.28).
narrative_ontology:measurement(agri_lock_grid_17, agricultural_contract_grower_lockin, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(agri_lock_grid_18, agricultural_contract_grower_lockin, stakes_inflation(class), 30, 0.68).
narrative_ontology:measurement(agri_lock_grid_19, agricultural_contract_grower_lockin, stakes_inflation(individual), 0, 0.65).
narrative_ontology:measurement(agri_lock_grid_20, agricultural_contract_grower_lockin, stakes_inflation(individual), 30, 0.84).
narrative_ontology:measurement(agri_lock_grid_21, agricultural_contract_grower_lockin, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(agri_lock_grid_22, agricultural_contract_grower_lockin, stakes_inflation(organizational), 30, 0.71).
narrative_ontology:measurement(agri_lock_grid_23, agricultural_contract_grower_lockin, stakes_inflation(structural), 0, 0.48).
narrative_ontology:measurement(agri_lock_grid_24, agricultural_contract_grower_lockin, stakes_inflation(structural), 30, 0.55).
narrative_ontology:measurement(agri_lock_grid_25, agricultural_contract_grower_lockin, suppression(class), 0, 0.55).
narrative_ontology:measurement(agri_lock_grid_26, agricultural_contract_grower_lockin, suppression(class), 30, 0.68).
narrative_ontology:measurement(agri_lock_grid_27, agricultural_contract_grower_lockin, suppression(individual), 0, 0.62).
narrative_ontology:measurement(agri_lock_grid_28, agricultural_contract_grower_lockin, suppression(individual), 30, 0.76).
narrative_ontology:measurement(agri_lock_grid_29, agricultural_contract_grower_lockin, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(agri_lock_grid_30, agricultural_contract_grower_lockin, suppression(organizational), 30, 0.8).
narrative_ontology:measurement(agri_lock_grid_31, agricultural_contract_grower_lockin, suppression(structural), 0, 0.42).
narrative_ontology:measurement(agri_lock_grid_32, agricultural_contract_grower_lockin, suppression(structural), 30, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_contract_grower_lockin, resource_allocation).
narrative_ontology:boltzmann_floor_override(agricultural_contract_grower_lockin, 0.18).
narrative_ontology:affects_constraint(agricultural_contract_grower_lockin, agricultural_labor_concentration).
narrative_ontology:affects_constraint(agricultural_contract_grower_lockin, rural_economic_dependency).
narrative_ontology:affects_constraint(agricultural_contract_grower_lockin, food_supply_chain_consolidation).

% DUAL FORMULATION NOTE:
% Contract grower lock-in is downstream of broader agricultural consolidation trends and market concentration. The upstream constraints (consolidation, supply-chain power) enable the lock-in mechanism (debt leverage, market control). Separate stories track the structural conditions (market concentration) from the mechanism (debt trap, pricing control). Network links show causality: consolidation → lock-in → rural economic collapse → dependence → suppressed resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_contract_grower_lockin, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
