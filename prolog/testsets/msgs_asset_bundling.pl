% ============================================================================
% CONSTRAINT STORY: msgs_asset_bundling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_msgs_asset_bundling, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: msgs_asset_bundling
 *   human_readable: Bundled Ownership of Knicks and Rangers under MSG Sports
 *   domain: economic/sports/corporate_structure
 *
 * SUMMARY:
 *   Madison Square Garden Sports (MSGS) bundles the New York Knicks (NBA) and
 *   New York Rangers (NHL) into a single publicly traded corporate entity.
 *   This bundling constraint creates extraction through forced combined
 *   ownership while simultaneously enabling coordination economies from
 *   integrated operations. The structural tension reveals itself across
 *   multiple agent perspectives: controlling shareholders experience pure
 *   coordination (Rope), minority shareholders face pure extraction (Snare),
 *   fan-consumers face bundled pricing extraction (Snare), alternative
 *   investors see mixed coordination-extraction (Tangled Rope), regulators
 *   see a potential unwinding target (Scaffold), sports league authorities
 *   exercise degraded oversight (Piton), and analytical observers debate
 *   whether bundling is fundamentally a coordination mechanism or an
 *   extraction device. The constraint exhibits high suppression (0.65) due to
 *   illiquid secondary markets, limited franchise availability, geographic
 *   concentration, and league approval mechanisms that resist structural
 *   change. Theater ratio (0.58) reflects that some operational integration
 *   genuinely occurs (coordination), but significant performative elements
 *   persist in financial communication and regulatory compliance that obscure
 *   the distributional consequences. Base extractiveness (0.52) indicates
 *   moderate-to-high extraction: the controlling stakeholder captures
 *   significant financial advantage through bundled control premium, while
 *   minority shareholders and fans bear costs of forced combined exposure and
 *   bundled pricing without clear benefit realization.
 *
 * KEY AGENTS:
 *   - MSG Ownership Class: Primary beneficiary (institutional/arbitrage) — controls bundled entity, captures coordination economies, extracts bundled premium from capital structure and pricing
 *   - Minority Shareholders: Primary victim (powerless/trapped) — locked into bundled asset exposure with no liquid exit, forced to accept capital allocation decisions of controlling shareholders
 *   - Fan-Consumer Base: Secondary victim (moderate/constrained) — faces bundled pricing (tickets, concessions, broadcast access) with limited alternatives and no transparent cost allocation between teams
 *   - Alternative Investor / Activist: Organized actor (organized/mobile) — could acquire separate assets, form competing structures, or pressure for unbundling; sees both coordination failure and extraction opportunity
 *   - Regulatory / Antitrust Authority: Organized actor (organized/arbitrage) — could mandate unbundling through enforcement action; views structure as subject to potential policy reversal
 *   - League Authorities (NBA/NHL): Institutional actor (organized/constrained) — approve ownership structure but exercise oversight performatively; constrained by franchise fee revenue dependence and tradition
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks pure coordination reading that ignores distributional harms or pure extraction reading that ignores genuine efficiency gains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(msgs_asset_bundling, 0.52).
domain_priors:suppression_score(msgs_asset_bundling, 0.65).
domain_priors:theater_ratio(msgs_asset_bundling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(msgs_asset_bundling, extractiveness, 0.52).
narrative_ontology:constraint_metric(msgs_asset_bundling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(msgs_asset_bundling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(msgs_asset_bundling, tangled_rope).
narrative_ontology:human_readable(msgs_asset_bundling, "Bundled Ownership of Knicks and Rangers under MSG Sports").
narrative_ontology:topic_domain(msgs_asset_bundling, "economic/sports/corporate_structure").

domain_priors:requires_active_enforcement(msgs_asset_bundling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, msgs_ownership_class).
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, controlling_stakeholder).
narrative_ontology:constraint_victim(msgs_asset_bundling, minority_shareholders).
narrative_ontology:constraint_victim(msgs_asset_bundling, fan_base_consumer_surplus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY SHAREHOLDER (SNARE) — Cannot exit without liquidating at illiquid secondary market prices. Forced to accept bundled asset exposure (Knicks + Rangers) with no mechanism to hedge individual team performance. Bears full extraction cost: capital locked into suboptimal allocation, voting power diluted, dividend policy controlled by majority. High suppression through limited exit and information asymmetry. No coordination benefit.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FAN-CONSUMER (SNARE) — Faces bundled pricing: ticket packages, concessions, merchandise, and broadcast access are bundled by MSGS. Cannot purchase Knicks access without also paying implicit bundle tax that subsidizes Rangers operations (or vice versa). Limited substitutes (other NYC venues serve different sports). Suppression high due to geographic and cultural lock-in. Extraction operates through bundled pricing mechanisms with no transparent cost allocation.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTROLLING STAKEHOLDER (ROPE) — Benefits from coordination across venue, operations, and capital allocation. Single equity structure reduces capital costs and increases financial flexibility. Can execute integrated strategies: cross-promote events, optimize scheduling, manage parking/hospitality across both teams. Arbitrage options abundant (spin-off either asset, maintain bundled structure, or exit at premium valuation). Views bundling as pure coordination benefit with minimal coercion overhead. Net beneficiary.
constraint_indexing:constraint_classification(msgs_asset_bundling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ALTERNATIVE INVESTOR (TANGLED ROPE) — Sees bundling as both coordination failure (could buy Knicks or Rangers separately at better value) and extraction mechanism (controlling shareholder locks in premium for bundled structure). Could form competing investment vehicle, acquire one asset separately, or pressure for unbundling. Mobile exit options create perspectival gap. Experiences mixed: some extraction (forced bundling premium), some coordination benefit (if integrated operations truly add value). Organized enough to challenge structure through shareholder activism or acquisition.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (SCAFFOLD) — Views bundling as a temporary coordination structure subject to antitrust scrutiny and potential unwinding. Sees clear extraction mechanics (pricing power, market concentration in NYC sports), but views unbundling as achievable policy goal with sunset clause. Could mandate separation, require transparency in cost allocation, or establish conditions under which bundling is permitted. Low theater ratio for this perspective: regulatory tools are direct and enforceable.
constraint_indexing:constraint_classification(msgs_asset_bundling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEAGUE AUTHORITY (PITON) — Has nominal oversight of franchise ownership and competitive balance rules, but exercises this authority largely performatively. League approval of MSGS structure was granted with minimal scrutiny. Rules around cross-ownership exist but are weak and rarely enforced. Theater_ratio high: league maintains appearance of governance while extracting franchise fees and playoff revenue regardless of bundling structure. Authority is degraded — inertial compliance more than active enforcement. Primary function (competitive integrity) atrophied; maintained through institutional inertia and tradition.
constraint_indexing:constraint_classification(msgs_asset_bundling, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — COORDINATION VIEW (ROPE) — Measures pure coordination benefits from integrated operations: shared venue infrastructure, unified front-office management, optimized scheduling, cross-promotion, consolidated marketing. Data support: MSGS has demonstrably reduced capital costs, improved operational efficiency, and enabled event calendar optimization that pure separate entities could not achieve. From this perspective, bundling is near-pure coordination with minimal extraction. Theater minimal, suppression low. However, this reading requires strong assumptions about ownership structure necessity and ignores distributional consequences.
constraint_indexing:constraint_classification(msgs_asset_bundling, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(msgs_asset_bundling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(msgs_asset_bundling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(msgs_asset_bundling, TR),
    TR >= 0.70.

:- end_tests(msgs_asset_bundling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The controlling shareholder captures bundled premium through several mechanisms: (1) Capital cost arbitrage — bundled structure reduces cost of capital below separate entities; (2) Strategic control — single equity governance structure concentrates voting power and limits minority influence; (3) Pricing power — bundled asset exposure allows extraction of implicit tax from fans and minority shareholders who cannot access individual assets. The value is not 0.70+ because genuine coordination economies do exist (shared venue, unified operations), reducing pure extraction component. Base extractiveness increased from 0.35 to 0.52 over the interval as MSGS consolidated operations and tightened cost allocations, shifting more value toward controlling shareholders. Suppression (0.65): High. Multiple suppression mechanisms: (1) Illiquid secondary trading in MSGS shares relative to separately traded teams; (2) Limited franchise availability — cannot buy Knicks or Rangers separately at liquid market prices; (3) League approval barriers — franchise transfers require league approval, preventing exit through rival ownership; (4) Geographic concentration — limited local alternatives; (5) Information asymmetry — bundled cost allocation is opaque to minority shareholders. Theater ratio (0.58): Moderate-high. Operational integration is real (shared venue, unified scheduling, consolidated marketing) but mixed with performative elements: (1) Financial communication emphasizes synergies without transparent accounting of cost allocation; (2) League governance appears to exercise oversight but approves bundling without scrutiny; (3) Investor relations materials present bundling as inevitable while concealing distributional consequences. Theater increased from 0.42 to 0.58 as MSGS invested more in integrated operations narrative while extraction mechanisms became more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   Controlling shareholders (Rope) experience bundling as pure coordination: integrated operations reduce costs, improve capital efficiency, and enable strategic flexibility unavailable to separate entities. Minority shareholders (Snare) experience bundling as pure extraction: forced combined exposure, illiquid exit, and voting power dilution without clear benefit. Fan-consumers (Snare) experience bundling as extraction through pricing: cannot access one team without implicit subsidy of the other through bundled package pricing. Alternative investors (Tangled Rope) see the gap itself: bundling creates coordination efficiency but also extraction opportunity — they could potentially acquire one asset separately or form competing structure that captures coordinated benefits without bundling tax. Regulatory authorities (Scaffold) see the structure as a temporary institutional arrangement subject to antitrust unwinding through sunset clause. League authorities (Piton) see their own oversight as degraded: they maintain nominal approval authority but exercise it performatively, constrained by franchise fee dependence and tradition rather than competitive integrity concerns. The analytical observer risks false neutrality: reading bundling as pure coordination ignores distributional harms to powerless agents; reading it as pure extraction ignores genuine efficiency gains. The true perspectival gap is between agents with arbitrage options (beneficiaries) and agents with trapped/constrained exits (victims).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the bundling constraint. The controlling shareholder has arbitrage options (could unbundle, maintain bundling, sell at premium) and benefits from constraint, yielding low d (around 0.10-0.20) and negative effective extraction chi — the constraint subsidizes them. Minority shareholders face trapped exit (illiquid, no alternatives) and bear costs, yielding high d (around 0.85-0.95) and high effective extraction chi. Fan-consumers face constrained exit (limited local alternatives, geographic concentration) and bear bundled pricing costs, yielding high d (around 0.75-0.85). Alternative investors with mobile exit options experience moderate d (around 0.50-0.60) and medium chi. Regulatory authorities with analytical access and arbitrage options (enforcement tools) experience low d. League authorities with constrained exit (revenue dependence) and degraded oversight power experience moderate d. The analytical observer's d is set by canonical analytical fallback (0.73) unless overridden. Engine derives d automatically from these positions plus beneficiary/victim declarations; directional overrides are not needed for this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question for MSG asset bundling is: 'Is this coordination with extraction or extraction dressed as coordination?' The base properties (ε=0.52, suppression=0.65) place the constraint firmly in Tangled Rope territory, which requires BOTH genuine coordination function AND asymmetric extraction. The evidence supports both: (1) Coordination genuine: Integrated venue operations, unified scheduling, consolidated front-office management demonstrably exist and would be difficult to achieve under separate ownership. Operational efficiency gains are real, even if difficult to quantify precisely. (2) Extraction genuine: Controlling shareholders capture bundled premium through capital structure arbitrage, pricing power, and voting concentration while minority shareholders and fans bear distributional costs. The constraint is NOT pure extraction (Snare) because coordination genuinely occurs. It is NOT pure coordination (Rope) because asymmetric extraction is systematically built into the structure. Tangled Rope classification is mandatrophy-resolved: the constraint combines both functions, and that combination creates the classification. The risk of false natural law framing (analytical observer reading as immutable coordination law) is mitigated by showing that separate franchise ownership exists elsewhere and generates comparable operational outcomes. The false summit detector should trigger: if the mountain classification were applied (claiming bundling is a law of sports economics), the evidence for equally successful separate franchises would contradict it. Unbundling is feasible (omega variable on regulatory feasibility), confirming that the structure is contingent institutional arrangement rather than natural law. The mandatrophy resolves through explicit recognition that Tangled Rope is the correct classification precisely because both coordination and extraction are structurally necessary to explain the observed phenomena.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What proportion of MSGS financial performance is attributable to integrated bundled operations versus what would occur under separate ownership structures?',
    'Comparable-company analysis: separately owned competing franchises (Warriors, Cavaliers, etc.) and their capital costs, operational margins, and strategic flexibility vs. MSGS performance metrics. Accounting decomposition of shared cost savings.',
    'If bundling explains > 15% of operating margin: genuine coordination function (Rope classification valid). If < 5%: bundling is primarily extraction mechanism (Snare classification). Between 5-15%: Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Attribution of MSGS performance gains to integrated bundling vs. separate operations').

omega_variable(
    minority_shareholder_exit_constraint,
    'Are minority shareholders genuinely trapped in bundled exposure or do liquid secondary markets and alternative hedging instruments provide sufficient exit?',
    'Liquidity analysis of MSGS secondary trading, bid-ask spreads, trading volume. Availability of options markets, short-sale capability, and derivative hedging for individual team exposure. Comparison to separately traded teams.',
    'If trapped (illiquid, no hedging): high suppression confirmed, snare classification strengthened. If mobile (liquid, hedgeable): exit options shift to ''mobile,'' perspectives reclassify, overall extractiveness declines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_shareholder_exit_constraint, empirical, 'Actual exit capacity for minority shareholders relative to bundling constraint').

omega_variable(
    bundled_pricing_cost_incidence,
    'What proportion of fan consumer costs (tickets, concessions, broadcast access) is attributable to bundled pricing mechanisms versus team-specific valuation?',
    'Price comparison: MSGS bundles vs. similar-quality separately owned franchises. Fan survey data on willingness-to-pay for bundled vs. unbundled access. Econometric estimation of bundle price premium.',
    'If bundle premium > 20% of consumer spend: extraction mechanism confirmed, fan-consumer snare classification strengthened. If < 5%: bundling may increase consumer welfare through coordination economies. Central empirical test for whether bundling is extraction or coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundled_pricing_cost_incidence, empirical, 'Consumer cost incidence of bundled pricing versus team-specific fundamentals').

omega_variable(
    regulatory_unwinding_feasibility,
    'Could antitrust action feasibly mandate unbundling of Knicks and Rangers without creating efficiency losses or asset destruction?',
    'Regulatory precedent analysis (forced breakups in telecom, energy, technology sectors). Assessment of MSGS operational interdependencies that would need restructuring. Estimation of transaction costs and stranded assets from separation.',
    'If high feasibility: scaffold perspective confirmed, regulatory pathway exists with limited sunk costs. If low feasibility (high interdependencies, stranded assets): unbundling becomes piton category (attempted dismantling that fails), regulatory sunset clause loses credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_unwinding_feasibility, conceptual, 'Regulatory feasibility of forced unbundling and efficiency consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(msgs_asset_bundling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msgs_tr_t0, msgs_asset_bundling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(msgs_tr_t10, msgs_asset_bundling, theater_ratio, 10, 0.53).
narrative_ontology:measurement(msgs_tr_t20, msgs_asset_bundling, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(msgs_be_t0, msgs_asset_bundling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(msgs_be_t10, msgs_asset_bundling, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(msgs_be_t20, msgs_asset_bundling, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(msgs_asset_bundling, resource_allocation).
narrative_ontology:affects_constraint(msgs_asset_bundling, nba_franchise_valuation).
narrative_ontology:affects_constraint(msgs_asset_bundling, nhl_franchise_valuation).

% DUAL FORMULATION NOTE:
% Asset bundling creates distinct constraints at different structural levels: (1) msgs_asset_bundling (this constraint) — the corporate governance structure binding two teams into single entity; (2) franchise_valuation constraints — how bundling affects individual franchise valuations independently. Bundling constraint is upstream because it creates the institutional framework within which franchise values are determined. Separate constraint stories for each franchise valuation would have different epsilon values reflecting team-specific empirical performance versus bundled structural premium.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
