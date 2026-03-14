% ============================================================================
% CONSTRAINT STORY: sovereign_debt_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_debt_sustainability, []).

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
 *   constraint_id: sovereign_debt_sustainability
 *   human_readable: Sovereign Debt Sustainability Constraint
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Sovereign debt sustainability creates a structural tension between
 *   creditor enforcement of repayment obligations and debtor nations'
 *   capacity to provide public services and maintain growth. The constraint
 *   exhibits strong perspectival variation: creditors experience it as
 *   coordination (enforcing predictable capital flows), debtors experience it
 *   as extraction (austerity enforcement without consent), future generations
 *   experience it as multi-decade snaring (opportunity cost of foregone
 *   investment), and international financial institutions experience it
 *   through identity-locked capture (their institutional mandate fused with
 *   creditor interests). Extractiveness has increased from 0.35 (2004-2008,
 *   pre-crisis) to 0.58 (post-2012, post-Eurozone crisis) as austerity
 *   regimes hardened. Theater ratio (0.55) reflects that debt sustainability
 *   enforcement is partially genuine coordination (preventing debt cascades,
 *   enabling refinancing) and partially performative (austerity doctrine
 *   inherited from gold-standard logic, applied despite weak empirical
 *   validation). The constraint is actively enforced through IMF
 *   conditionality, rating agency downgrades, and creditor coordination.
 *
 * KEY AGENTS:
 *   - Debtor Populations: Primary victim (powerless/trapped) — bear austerity costs (reduced services, wage suppression) with no exit or voice
 *   - Future Generations: Primary victim (powerless/trapped, generational horizon) — inherit debt obligations and foregone public investment; trapped for decades
 *   - Debtor Nation Governments: Mixed actor (organized/constrained) — must coordinate fiscal sustainability while enforcing extraction on own populations; constrained by creditor conditionality
 *   - Creditor Nations and Institutional Investors: Primary beneficiary (institutional/arbitrage) — receive reliable payment flows and can diversify creditor exposure; arbitrage options enable exit
 *   - IMF and World Bank: Captured institutional actors (institutional/identity_locked) — organizational identity fused with creditor interests; unable to perceive growth-first alternatives despite empirical evidence
 *   - Debt Relief Coalitions: Organized challengers (organized/constrained) — Paris Club, HIPC, debt-for-climate structures; building exit pathways through restructuring and alternative financing
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical) — tempted to see debt constraints as immutable laws rather than contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_debt_sustainability, 0.58).
domain_priors:suppression_score(sovereign_debt_sustainability, 0.68).
domain_priors:theater_ratio(sovereign_debt_sustainability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_debt_sustainability, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereign_debt_sustainability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sovereign_debt_sustainability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_debt_sustainability, tangled_rope).
narrative_ontology:human_readable(sovereign_debt_sustainability, "Sovereign Debt Sustainability Constraint").
narrative_ontology:topic_domain(sovereign_debt_sustainability, "economic/geopolitical").

domain_priors:requires_active_enforcement(sovereign_debt_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability, creditor_nations).
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability, international_financial_institutions).
narrative_ontology:constraint_beneficiary(sovereign_debt_sustainability, institutional_investors).
narrative_ontology:constraint_victim(sovereign_debt_sustainability, debtor_populations).
narrative_ontology:constraint_victim(sovereign_debt_sustainability, future_generations).
narrative_ontology:constraint_victim(sovereign_debt_sustainability, public_services_provision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR POPULATION (SNARE) — Trapped by debt-driven austerity constraints. Citizens experience reduced public services, wage suppression, and pension cuts with no exit option. Trapped in the constraint's extraction mechanism — bear all costs while having no meaningful voice in debt restructuring decisions. Maximum experienced extraction.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Trapped by inherited debt obligations and delayed climate/infrastructure investment. Fiscal austerity constraints foreclose long-term public investment even when economically rational. Cannot exit; bear extraction across decades through opportunity costs.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DEBTOR NATION GOVERNMENT (TANGLED ROPE) — Constrained by debt service requirements and IMF/World Bank conditionality, but also coordinating among competing constituencies. Government experiences genuine coordination (managing fiscal sustainability, preventing default cascade) alongside mandatory extraction (creditor enforcement of austerity, loss of fiscal sovereignty). Active enforcement by creditors; constrained exit through refinancing cycles.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDITOR NATIONS AND INSTITUTIONS (ROPE) — Experience the constraint as pure coordination: debt sustainability requirements enforce reliable payment flows, enabling capital reallocation. Creditors have arbitrage options (creditor substitution, currency selection, geographical diversification). Net beneficiary position. Extraction flows toward these agents but is experienced as legitimate contract enforcement.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEBT RELIEF COALITIONS (SCAFFOLD) — Organized agents (IMF structural reforms, Paris Club renegotiations, HIPC Initiative, debt-for-climate swaps) frame the constraint as temporary. Sunset logic: debt relief programs, growth-oriented fiscal reforms, and alternative financing (green bonds, climate finance) are creating exit pathways. Theater relatively low — negotiated restructuring creates real fiscal space, not performative relief. Constrained by creditor resistance but with organized agency.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: IMF/WORLD BANK (IDENTITY-LOCKED) — Institutional actors structurally positioned to enforce debt sustainability but whose organizational identity has fused with creditor interests. These organizations see their own mandate as naturally aligned with austerity enforcement, unable to perceive growth-first alternatives as legitimate despite empirical evidence. Captured not by external force but by internalized institutional framing (creditor-dominant governance, staff trained in orthodox macroeconomics, organizational reputation tied to creditor confidence). Identity-locked exit prevents them from seeing structural alternatives.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: GOLD STANDARD LOGIC (PITON) — Civilizational perspective reveals that debt sustainability doctrine inherits the logic of commodity-backed currency constraints (gold standard mentality): the belief that governments must maintain external balance sheets like households. This theater persists despite decades of post-Bretton Woods evidence that sovereign currency-issuing governments have different budget constraints than currency-users. The performative logic (governments must balance budgets like households) sustains austerity despite weak empirical grounding. Institutional inertia maintains the ritual.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN RISK) — Risk perspective from civilizational/universal scope: naturalizing debt sustainability as an immutable economic law ('governments must maintain solvency'). This is a false summit — debt sustainability is a contingent institutional arrangement, not a natural law. Currency-issuing sovereigns face different constraints than households. The appearance of naturalness comes from decades of institutional embedding and theoretical monopoly by orthodox macroeconomics. Engine will flag this as false summit.
constraint_indexing:constraint_classification(sovereign_debt_sustainability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_debt_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_debt_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_debt_sustainability, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_debt_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_debt_sustainability, TR),
    TR >= 0.70.

:- end_tests(sovereign_debt_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. The constraint extracts significantly from debtor populations through austerity but is not as severe as a pure snare because some coordination genuinely occurs (creditors do enforce payment discipline, which prevents default cascades and enables capital reallocation). The constraint has tightened over the measurement interval, driven by: (1) Eurozone crisis hardening austerity doctrine; (2) concentration of creditor power through IMF/ECB dominance; (3) rating agency downgrades that lock debtor nations into refinancing at higher cost. Growth-first fiscal reforms would likely reduce extractiveness by improving debt-to-GDP ratios through expansion rather than contraction. Suppression (0.68): Very high. Barriers to exit include: external debt denominated in creditor currencies (no unilateral write-down option for currency-users); capital flight risk (exit attempts trigger immediate punishment through capital outflows); institutional investor coordination (creditors move together on downgrades); political conditionality (IMF demands governance reform tied to austerity). Theater ratio (0.55): Moderate. Debt sustainability enforcement has partial reality (creditor discipline prevents certain kinds of fiscal recklessness) but also performative elements (austerity doctrine persists despite evidence that growth-oriented fiscal stimulus improves debt-to-GDP ratios in demand-limited economies; household budget constraint analogy to government budgets is theoretically indefensible yet politically dominant). The theater has been stable rather than declining, despite growing empirical challenges to orthodox macroeconomics.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. Creditors see pure coordination (Rope) — debt discipline enables capital mobility and prevents moral hazard. Debtor populations see pure extraction (Snare) — they bear austerity costs without input or exit. Debtor governments see mixed coordination-extraction (Tangled Rope) — they must coordinate fiscal sustainability while enforcing extraction on their own citizens. Debt relief organizations see temporary dysfunction with a sunset (Scaffold) — alternative financing and restructuring pathways are building real exits. The IMF/World Bank occupy a unique position as captured institutional actors (identity-locked) — their organizational identity is constitutively fused with creditor-first macroeconomics, preventing them from perceiving growth-oriented alternatives as legitimate despite staff research validating them. The civilizational analytical perspective risks naturalizing this as immutable (Mountain) — the hidden assumption being that all governments face household-like budget constraints. This false summit is unmasked by comparing currency-issuing sovereigns (which face different constraints) to currency-users, revealing that debt sustainability doctrine is an inherited institutional framework, not an economic law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Creditors have low d (0.05-0.15) because they are beneficiaries with arbitrage options (creditor substitution, currency choice, diversification) — they experience low effective extraction f(d) because the constraint extracts wealth toward them. Debtor populations have high d (0.90-0.98) because they are victims trapped by external debt denominated in creditor currencies with no unilateral exit option — they experience maximum f(d) because extraction runs maximally away from them. Debtor governments have mid-range d (0.50-0.65) because they are both beneficiaries (preventing default cascade) and victims (creditor conditionality limits fiscal space) — constrained exit options and mixed structural position produce moderate d. Organized debtor coalitions have lower d (0.40-0.55) because their agency reduces experienced extraction — they can negotiate with creditors, though not escape entirely. IMF/World Bank occupy d=0.35-0.45 despite being institutional beneficiaries (interest-bearing claims on debtors) because their identity-locked status creates internal contradiction — they cannot exercise their beneficiary position's arbitrage optionality because their identity is fused with 'creditor-serving macroeconomics' framing. The engine's computation of χ from ε × f(d) × σ(S) will show: creditors experience low χ (extraction subsidizes them); debtors experience high χ (maximum experienced extraction); organizations experience χ that reflects their paradoxical capture (institutional power but identity-locked to one side of the extraction flow).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CASE: Sovereign debt sustainability resolves as Tangled Rope through the requirement of BOTH genuine coordination AND asymmetric extraction. Genuine coordination element: creditors enforcing discipline does prevent default cascades and enables capital reallocation — this is a real collective action problem that the debt sustainability regime solves. Asymmetric extraction element: the regime disproportionately extracts from debtor populations through austerity while concentrating benefits to creditors — this is structurally embedded, not incidental. The constraint fails to be pure Rope because the coordination benefit flows primarily to creditors (they get reliable payments) while the extraction cost is borne by debtors (austerity reduces their public services and growth). It fails to be pure Snare because genuine coordination prevents worse outcomes (default, capital flight, fiscal instability). The mandatrophy resolves by showing that: (1) the coordination claim is empirically validated (debt discipline does reduce default risk); (2) the extraction claim is also validated (austerity reduces growth and increases debt-to-GDP in demand-limited economies, which is extraction inefficiency); (3) both cannot be optimized simultaneously — tighter discipline increases repayment certainty but also increases extraction cost through growth forgone; (4) the ratio between coordination and extraction (theater ratio 0.55) reflects that roughly half the institutional apparatus is genuine (creditor discipline) and half is performative (austerity doctrine applied despite weak theory). The false natural law (Mountain perspective) is rejected because debt sustainability constraints are institutional arrangements (post-Bretton Woods, IMF-governed, creditor-coordinated), not immutable economic laws. Currency-issuing sovereigns face different constraints than currency-users or commodity-backed systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    currency_sovereignty_threshold,
    'Does currency-issuing sovereignty fundamentally alter debt sustainability constraints, or is the orthodox orthodoxy (governments face household-like budget constraints) empirically validated?',
    'Comparative historical analysis of modern monetary systems: debt-to-GDP trajectories for currency-issuers vs currency-users; correlation between debt levels and inflation/currency crises; post-Bretton Woods evidence on fiscal space',
    'If currency sovereignty matters: snare classification is too severe (constrained agents have more fiscal agency); austerity is ideological choice, not structural necessity. If orthodox view correct: snare classification stands; debt constraints are real and binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(currency_sovereignty_threshold, empirical, 'Whether currency sovereignty fundamentally alters debt constraints').

omega_variable(
    austerity_growth_causality,
    'Does austerity enforce debt sustainability through credible repayment capacity (growth mechanism), or does it undermine growth and debt-to-GDP ratios (contraction mechanism)?',
    'Econometric analysis of austerity episodes: growth rates before/after; debt-to-GDP trajectory post-austerity; debt service burden dynamics; comparison to counterfactual scenarios with continued fiscal stimulus',
    'If growth mechanism dominates: creditors'' rope perspective is correct (austerity improves repayment capacity). If contraction mechanism dominates: austerity increases extraction inefficiency (victims bear costs while debt-to-GDP worsens) — the constraint becomes a snare masquerading as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(austerity_growth_causality, empirical, 'Whether austerity improves or undermines debt-to-GDP ratios').

omega_variable(
    creditor_collective_action,
    'Do creditors act as a coherent enforcer of debt sustainability, or do they compete (driving down enforcement), enabling debtor negotiating power?',
    'Institutional analysis of creditor coordination: IMF governance concentration, Paris Club flexibility, bond-holder holdout rates, emergence of alternative creditors (China, development banks); evidence on enforcement consistency across debtor nations',
    'If coordinated: creditor rope perspective stands and extraction is stable. If fragmented: debtor nations have de facto arbitrage options (creditor substitution); tangled rope classification may shift toward rope from debtor perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_collective_action, empirical, 'Whether creditors act as coherent enforcer or compete').

omega_variable(
    identity_lock_reversibility,
    'Is IMF/World Bank organizational capture (identity-locked) to orthodox macroeconomics reversible, or is the institutional identity constitutively fused with creditor interests?',
    'Organizational history of reform attempts; staff mobility patterns; research culture shifts; governance reform effectiveness; comparison to peer organizations that have shifted policy paradigms',
    'If reversible: identity-locked exit classification is too pessimistic (structural change is possible); scaffold perspective is conservative. If constitutive: organizational capture is durable; alternatives must bypass these institutions (climate finance, bilateral arrangements).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether institutional capture is reversible').

omega_variable(
    future_generation_discounting,
    'Is debt-driven opportunity cost to future generations (foregone education, climate adaptation, infrastructure) quantifiable as structural extraction, or is it a philosophical concern outside economic constraint models?',
    'Intergenerational accounting: net present value of foregone investment flows; climate cost projections; education attainment impacts of austerity; long-term growth differentials under different fiscal regimes',
    'If quantifiable: future generation snare is structurally real and extractiveness should be rated higher. If philosophical: constraint applies only to present-generation debtor populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_discounting, empirical, 'Quantifiability of intergenerational extraction').

omega_variable(
    alternative_financing_maturity,
    'Are debt-for-climate swaps, green bonds, and alternative creditor arrangements (BRICS, ADB) sufficiently mature and scalable to constitute a real sunset clause, or are they marginal alternatives?',
    'Capital flow volume analysis: green bond issuance vs traditional sovereign bonds; debt-for-climate swap volumes; alternative creditor lending patterns; fiscal space impact of debt restructuring under alternative arrangements',
    'If mature/scalable: scaffold perspective is structurally grounded; sunset is real and theater ratio should be lower. If marginal: scaffold is aspirational; tangled rope likely persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_maturity, empirical, 'Maturity and scalability of alternative financing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_debt_sustainability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sds_tr_t0, sovereign_debt_sustainability, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sds_tr_t10, sovereign_debt_sustainability, theater_ratio, 10, 0.52).
narrative_ontology:measurement(sds_tr_t20, sovereign_debt_sustainability, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(sds_be_t0, sovereign_debt_sustainability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sds_be_t10, sovereign_debt_sustainability, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sds_be_t20, sovereign_debt_sustainability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_debt_sustainability, resource_allocation).
narrative_ontology:boltzmann_floor_override(sovereign_debt_sustainability, 0.12).
narrative_ontology:affects_constraint(sovereign_debt_sustainability, austerity_policy_regime).
narrative_ontology:affects_constraint(sovereign_debt_sustainability, emerging_market_currency_crises).
narrative_ontology:affects_constraint(sovereign_debt_sustainability, climate_finance_debt_trap).

% DUAL FORMULATION NOTE:
% Sovereign debt sustainability decomposes into structurally distinct constraints at different empirical scopes. This story models the aggregate institutional regime (IMF-coordinated debt discipline across debtor nations). Downstream constraints include austerity_policy_regime (ε=0.42, specific national-level austerity enforcement), emerging_market_currency_crises (ε=0.68, sharp episodes of capital flight and currency collapse triggered by debt sustainability concerns), and climate_finance_debt_trap (ε=0.55, structural coupling where debt service obligations crowd out climate adaptation investment). Each story has distinct ε values reflecting different observables: this story's ε reflects the institutional burden of debt servicing; downstream stories reflect policy implementation (austerity) and crisis dynamics (currency collapse).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_debt_sustainability, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
