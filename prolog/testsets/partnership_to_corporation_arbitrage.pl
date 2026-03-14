% ============================================================================
% CONSTRAINT STORY: partnership_to_corporation_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_partnership_to_corporation_arbitrage, []).

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
 *   constraint_id: partnership_to_corporation_arbitrage
 *   human_readable: Partnership-to-Corporation Arbitrage in Multi-Jurisdictional Business Structures
 *   domain: corporate_law/business_structure/tax_policy
 *
 * SUMMARY:
 *   Partnership-to-corporation arbitrage exploits the differential tax
 *   treatment of pass-through entities (partnerships) versus incorporated
 *   entities across multi-jurisdictional tax regimes. A partnership
 *   structured in a low-tax jurisdiction can convert income to corporate form
 *   (or be treated as a corporation for tax purposes in high-tax
 *   jurisdictions while retaining partnership classification elsewhere) to
 *   optimize overall tax liability. This constraint demonstrates a genuine
 *   mixed mechanism: legitimate multi-jurisdictional business coordination
 *   coexists with systematic profit-shifting extraction. The small business
 *   partner trapped in the structure bears liability exposure without capture
 *   of the arbitrage benefit; the multinational parent benefits from tax
 *   optimization; tax authorities see erosion of their base; international
 *   harmonization initiatives are building scaffolding to constrain the
 *   mechanism while permanent solutions mature. The theater ratio (0.48)
 *   reflects that compliance activities (documentation, beneficial ownership
 *   declarations, substance-over-form audits) are partially performative —
 *   they create the appearance of constraint without fully preventing
 *   arbitrage.
 *
 * KEY AGENTS:
 *   - Small Business Partners: Primary victims (powerless/trapped) — bear liability exposure and lose arbitrage benefits; cannot restructure without total loss
 *   - Mid-Tier Partnership Stakeholders: Secondary victims (moderate/constrained) — benefit from operational coordination but face extraction through asymmetric profit allocation
 *   - Multinational Corporation Parent: Primary beneficiary (institutional/arbitrage) — captures tax optimization gains, high exit optionality across jurisdictions
 *   - Tax Authorities (Coalition): Organized institutional actor (organized/mobile) — see both coordination function (need cross-border structures) and extraction (base erosion); generational time horizon
 *   - Tax Arbitrage Intermediaries: Secondary beneficiary (institutional/arbitrage) — professional service providers (accountants, law firms) who design structures; economically aligned with multinational parents
 *   - Legacy Partnership Law Framework: Institutional actor (institutional/arbitrage) — persists through inertia while actual arbitrage mechanisms circumvent its intent; performative compliance substitute for real constraint
 *   - International Tax Harmonization Initiatives: Organized coalition (organized/constrained) — OECD Pillar Two, BEPS, country-by-country reporting building temporary scaffolding toward permanent solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(partnership_to_corporation_arbitrage, 0.58).
domain_priors:suppression_score(partnership_to_corporation_arbitrage, 0.65).
domain_priors:theater_ratio(partnership_to_corporation_arbitrage, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(partnership_to_corporation_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(partnership_to_corporation_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(partnership_to_corporation_arbitrage, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(partnership_to_corporation_arbitrage, tangled_rope).
narrative_ontology:human_readable(partnership_to_corporation_arbitrage, "Partnership-to-Corporation Arbitrage in Multi-Jurisdictional Business Structures").
narrative_ontology:topic_domain(partnership_to_corporation_arbitrage, "corporate_law/business_structure/tax_policy").

domain_priors:requires_active_enforcement(partnership_to_corporation_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(partnership_to_corporation_arbitrage, multinational_corporations).
narrative_ontology:constraint_beneficiary(partnership_to_corporation_arbitrage, tax_arbitrage_intermediaries).
narrative_ontology:constraint_victim(partnership_to_corporation_arbitrage, partnership_stakeholders).
narrative_ontology:constraint_victim(partnership_to_corporation_arbitrage, small_business_participants).
narrative_ontology:constraint_victim(partnership_to_corporation_arbitrage, host_country_tax_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL BUSINESS PARTNER (SNARE) — Trapped in partnership structure by capital investment, relational equity, and limited exit without total loss. Cannot restructure without triggering tax events or dissolving relationships. Bears extraction through liability exposure while multinational parent captures profits. Maximum extraction experienced — powerless + trapped = trapped agent sees mountain-level immutability.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER PARTNERSHIP STAKEHOLDER (TANGLED ROPE) — Constrained by switching costs and regulatory complexity but retains some organizational capacity. Benefits from partnership structure for operational coordination but bears extraction through profit allocation asymmetries. Can theoretically exit but at significant cost. Mixed coordination-extraction mechanism.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATION (ROPE) — Perceives partnership structure as coordination mechanism for managing multi-jurisdictional operations, regulatory compliance, and risk distribution. High exit optionality (arbitrage across tax regimes, jurisdictions). Net beneficiary from profit allocation and tax-optimization mechanisms. Experiences constraint as legitimate coordination.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COALITION OF TAX AUTHORITIES (TANGLED ROPE) — Organized institutional actors with generational time horizon see partnership-to-corporation arbitrage as both coordination mechanism (need for cross-border business structures) and extraction (profit shifting erodes local tax base). Mobile in principle (can change regulations, establish treaties) but constrained by coordination problems between jurisdictions. Sees genuine extraction but also recognizes coordination function.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PARTNERSHIP LAW FRAMEWORK (PITON) — Historical partnership classification systems (pass-through entities, beneficial ownership rules, partnership anti-abuse provisions) are largely performative relative to modern financial engineering. The regulatory framework persists through institutional inertia while actual mechanisms circumvent its intent. Theater ratio high: compliance activities (documentation, reporting) substitute for real constraint on arbitrage. The framework is maintained because alternatives haven't fully replaced it, not because it functions as designed.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL TAX HARMONIZATION INITIATIVES (SCAFFOLD) — Emerging frameworks (OECD Pillar Two, BEPS initiatives, country-by-country reporting) function as temporary scaffolding to constrain partnership arbitrage while permanent solutions (full coordination on corporate taxation, unitary taxation standards) mature. These initiatives have sunset logic: as global minimum tax norms and reporting standardization increase, the partnership-corporation distinction becomes less exploitable. Organized coalition with constrained exit (enforcement gaps remain, but direction is toward tighter integration).
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY FRICTION VIEW (MOUNTAIN) — From civilizational/universal perspective, partnership-to-corporation arbitrage reflects an immutable tension: sovereign nations cannot credibly commit to mutual taxation without enforcement mechanisms, and enforcement mechanisms cannot credibly constrain multinational capital structures because exit options (relocation, restructuring, asset transfer) are always available. The constraint is seen as a natural law of multi-jurisdictional economies. However, this risks naturalizing what is contingent institutional design.
constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(partnership_to_corporation_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(partnership_to_corporation_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(partnership_to_corporation_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(partnership_to_corporation_arbitrage, TR),
    TR >= 0.70.

:- end_tests(partnership_to_corporation_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The partnership-to-corporation arbitrage mechanisms generate meaningful tax asymmetries that flow to multinational parents and away from small business partners and host-country tax bases. The extractiveness is not maximal (0.72+) because legitimate business coordination is genuinely present — multi-jurisdictional operations do require complex structures — and the arbitrage exists within legal frameworks (not outright fraud). The value reflects that extraction coexists with real coordination value, creating a hybrid mechanism. Suppression (0.65): High. Barriers to resistance include: (1) technical complexity of tax code and partnership regulations make exit/restructuring extremely costly; (2) relational barriers — partners are often embedded in long-term relationships; (3) information asymmetry — small business partners often lack capacity to detect or understand the arbitrage mechanism; (4) regulatory enforcement gaps — detection requires sophisticated auditing and cross-border coordination that many tax authorities lack. Theater ratio (0.48): Moderate. Compliance activities (beneficial ownership reporting, country-by-country reporting, partnership anti-abuse documentation) create the appearance of constraint without fully preventing arbitrage. New structures are designed around new regulations faster than regulations can close them. The theater has increased over the interval as regulations tighten but innovation keeps pace.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a seven-way perspectival split. The trapped small business partner sees a snare — an inescapable extraction mechanism with maximum experienced cost. The constrained mid-tier stakeholder sees tangled rope — genuine operational coordination mixed with profit extraction. The institutional multinational parent sees rope — a legitimate coordination mechanism for managing global operations. The organized coalition of tax authorities sees tangled rope — both a coordination necessity and a profit-shifting extraction problem. The legacy regulatory framework sees itself as degraded (piton) — it performs compliance theater without preventing arbitrage. The international harmonization initiatives see a temporary scaffold — OECD coordination is building constraints that will eventually make partnership arbitrage uneconomical. The civilizational analytical observer risks seeing a natural law — the inherent friction between sovereign tax systems and mobile capital — but this naturalizes what is contingent institutional design. The perspectival gap between beneficiary (rope) and victim (snare) reveals the core extraction: the same structure that is coordination for the multinational parent is pure extraction for the small business partner.
 *
 * DIRECTIONALITY LOGIC:
 *   The small business partner sees maximum extraction (trapped + powerless + no arbitrage access) = d ≈ 0.95 = f(d) ≈ 1.42 = high experienced chi. The mid-tier stakeholder sees moderate extraction (constrained + moderate power + some operational benefit) = d ≈ 0.60 = f(d) ≈ 0.80 = moderate chi. The multinational parent sees negative extraction / coordination benefit (arbitrage + institutional power + beneficiary) = d ≈ 0.15 = f(d) ≈ -0.01 = low/negative chi. Tax authorities see intermediate extraction (mobile + organized + mixed beneficiary/victim) = d ≈ 0.55 = f(d) ≈ 0.75 = moderate-high chi. The directionality gap reveals the core extraction mechanism: the constraint benefits those with arbitrage options (multinational corporations, intermediaries) and harms those locked into local partnership structures (small business participants, host countries).
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID MECHANISM — This constraint resolves mandatrophy by showing that partnership-to-corporation arbitrage is genuinely tangled: it is BOTH a coordination solution for legitimate multi-jurisdictional business AND an extraction mechanism that asymmetrically benefits multinational parents and harms small partnership stakeholders. The mandatrophy cannot be resolved by declaring it purely one or the other. (1) COORDINATION CLAIM: Multi-jurisdictional operations genuinely require complex structures to handle different regulatory regimes, risk distribution, and operational complexity. Partnership structures provide flexibility that pure incorporation cannot. (2) EXTRACTION CLAIM: The arbitrage mechanism deliberately exploits differential tax treatment to shift profits from high-tax to low-tax jurisdictions, eroding host-country tax bases and concentrating benefits on actors with sufficient capital and sophistication to engineer the structures. Both claims are true simultaneously. The constraint is tangled because the coordination function is real but is paired with asymmetric extraction. The beneficiary (multinational parent) sees genuine coordination value and pays real costs of compliance complexity. The victim (small business partner, host-country tax base) sees the extraction without the coordination benefit. The scaffold perspective (international harmonization) suggests a potential resolution: as global tax norms align and reporting standardizes, the arbitrage advantage decays while legitimate coordination capacity persists (different regimes still exist, structures are still needed, but the profit-shifting gap closes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficial_ownership_chain_depth,
    'How many layers of beneficial ownership opacity (shell corporations, trusts, nominee partners, offshore entities) are necessary to make the partnership-to-corporation arbitrage structurally viable versus merely tax-efficient?',
    'Empirical analysis of successful arbitrage structures: measure minimum layers required for enforcement evasion vs. layers observed in actual structures. Compare to legitimate multi-jurisdictional business operations.',
    'If depth < 3 layers: legitimate business complexity. If depth > 5 layers: structural fraud indicator, shifts classification toward snare (intentional obfuscation mechanism). If depth is variable: extraction mechanism is intentional opacity design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_ownership_chain_depth, empirical, 'Opacity depth threshold for enforcement evasion').

omega_variable(
    economic_substance_threshold,
    'What proportion of partnership income must derive from genuine operational activity (vs. pure arbitrage pass-through) before the structure ceases to be extractive?',
    'Tax authority audits, revenue department analysis of partnership-to-corporation conversions; correlation between stated business purpose and actual income sources.',
    'If threshold < 30%: most partnership structures are primarily extraction vehicles. If threshold > 70%: arbitrage is minor side effect of legitimate business structure. If threshold is unclear: economic substance doctrine fails as a gating mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_substance_threshold, empirical, 'Proportion of operational vs. arbitrage income').

omega_variable(
    coordinated_enforcement_ceiling,
    'Can international tax authority coordination (OECD, multilateral treaties, information exchange) ever achieve sufficient synchronization to prevent arbitrage through structural asynchrony?',
    'Historical analysis of major tax harmonization efforts; comparison of audit timing, information availability, and enforcement capacity across jurisdictions before and after coordination initiatives.',
    'If coordination succeeds: scaffold classification confirmed, sunset is real, arbitrage mechanisms weaken as reporting tightens. If coordination fails: structural deficit is permanent, arbitrage is feature not bug, classification shifts toward snare (systemic extraction design).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_enforcement_ceiling, empirical, 'Feasibility of coordinated enforcement').

omega_variable(
    innovation_rate_vs_regulation_lag,
    'How quickly do new partnership-to-corporation arbitrage structures emerge relative to regulatory closure? Does the constraint become a Red Queen race (regulation always one step behind)?',
    'Timeline analysis of arbitrage structure discovery vs. regulatory response; patent filings for tax-planning strategies; lag time from closure to workaround.',
    'If lag < 2 years: regulation can keep pace, systemic control is feasible. If lag > 5 years: regulation permanently reactive, arbitrage fundamentally moves faster than law. If lag is accelerating: constraint degrades over time (piton signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_rate_vs_regulation_lag, empirical, 'Regulatory closure vs. innovation lag rate').

omega_variable(
    power_concentration_feedback,
    'Does partnership-to-corporation arbitrage generate competitive advantage that concentrates corporate power (large firms can afford better tax planning), which then increases capacity for further arbitrage?',
    'Correlation analysis: firm size vs. effective tax rate, M&A concentration pre/post major arbitrage closure, profit margin differentials between well-resourced multinationals and small partnerships.',
    'If positive feedback exists: constraint is self-reinforcing extraction mechanism, classification more snare-like. If feedback is weak: arbitrage is dispersed, competitive pressure limits extraction, more rope-like. If feedback is reversing: scaffold initiatives are breaking the cycle, sunset is operational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_concentration_feedback, empirical, 'Power-concentration feedback loop in arbitrage capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(partnership_to_corporation_arbitrage, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p2c_tr_t0, partnership_to_corporation_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(p2c_tr_t7, partnership_to_corporation_arbitrage, theater_ratio, 7, 0.42).
narrative_ontology:measurement(p2c_tr_t15, partnership_to_corporation_arbitrage, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(p2c_be_t0, partnership_to_corporation_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(p2c_be_t7, partnership_to_corporation_arbitrage, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(p2c_be_t15, partnership_to_corporation_arbitrage, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(partnership_to_corporation_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(partnership_to_corporation_arbitrage, transfer_pricing_arbitrage).
narrative_ontology:affects_constraint(partnership_to_corporation_arbitrage, beneficial_ownership_opacity).
narrative_ontology:affects_constraint(partnership_to_corporation_arbitrage, double_irish_dutch_sandwich_structures).

% DUAL FORMULATION NOTE:
% Partnership-to-corporation arbitrage is upstream of specific tax avoidance structures (transfer pricing, beneficial ownership hiding, double Irish arrangements). Each specific structure has its own ε reflecting its particular compliance risk and innovation rate; this story captures the general constraint on partnership/corporation classification boundaries that enables all specific variants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(partnership_to_corporation_arbitrage, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
