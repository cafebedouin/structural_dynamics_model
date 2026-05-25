% ============================================================================
% CONSTRAINT STORY: provincial_tax_collection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_tax_collection, []).

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
 *   constraint_id: provincial_tax_collection
 *   human_readable: Provincial Tax Collection System
 *   domain: fiscal_governance/political_economy
 *
 * SUMMARY:
 *   Provincial tax collection systems exhibit the structural tension between
 *   genuine public-goods coordination and extraction/rent-seeking. All
 *   populations require some mechanism to fund collective infrastructure
 *   (roads, courts, education, healthcare), creating a coordination problem
 *   that taxation solves. Simultaneously, the taxing authority has structural
 *   advantages that enable extraction: power to set rates, power to audit,
 *   power to penalize non-compliance, and monopoly on legitimate coercion.
 *   The extractiveness value (0.52) reflects a moderate system that includes
 *   real coordination (public goods provision) alongside real extraction
 *   (asymmetric burden distribution, bureaucratic overhead, preferential
 *   treatment for organized actors). The temporal trajectory shows increasing
 *   extractiveness and theater ratio, indicating that as systems mature,
 *   rent-seeking and compliance theater accumulate faster than genuine public
 *   goods provision. The constraint exhibits all six classification types
 *   depending on observer position: wage earners perceive snare (trapped with
 *   no coordination benefit), small business experiences tangled rope (mixed
 *   benefits and costs), governments experience rope (coordination
 *   mechanism), mobile capital experiences tangled rope (both coordination
 *   and extraction), bureaucracy appears as piton (degraded ritual), and
 *   wealth holders extract while also benefiting from coordination (tangled
 *   rope). The analytical observer's mountain perspective risks naturalizing
 *   what is a contingent political arrangement.
 *
 * KEY AGENTS:
 *   - Wage Earner: Primary victim (powerless/trapped) — mandatory withholding, cannot exit jurisdiction without exiting employment, bears disproportionate compliance burden
 *   - Small Business Owner: Secondary victim (moderate/constrained) — faces relocation costs, licensing barriers, dependent customer base; also benefits from tax-funded infrastructure
 *   - Provincial Government: Primary beneficiary (institutional/arbitrage) — captures revenue authority; can arbitrage fiscal federalism; uses taxation to solve public-goods collective action problem
 *   - Mobile Capital Sector: Secondary beneficiary-victim (organized/mobile) — can exit to favorable jurisdictions; extracts through tax competition and special deals; benefits from rule of law and infrastructure
 *   - Wealth Holder: Tertiary beneficiary-victim (powerful/mobile) — highest exit capacity; can arbitrage globally; extracts through tax planning while relying on property-rights protection
 *   - Tax Administration Bureaucracy: Institutional actor (institutional/arbitrage) — maintains compliance theater; perpetuates complexity as justification; benefits from system continuation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent political arrangement as inherent to statecraft
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_tax_collection, 0.52).
domain_priors:suppression_score(provincial_tax_collection, 0.68).
domain_priors:theater_ratio(provincial_tax_collection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_tax_collection, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_tax_collection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(provincial_tax_collection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_tax_collection, tangled_rope).
narrative_ontology:human_readable(provincial_tax_collection, "Provincial Tax Collection System").
narrative_ontology:topic_domain(provincial_tax_collection, "fiscal_governance/political_economy").

domain_priors:requires_active_enforcement(provincial_tax_collection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_tax_collection, provincial_government).
narrative_ontology:constraint_beneficiary(provincial_tax_collection, bureaucratic_apparatus).
narrative_ontology:constraint_victim(provincial_tax_collection, taxpaying_population).
narrative_ontology:constraint_victim(provincial_tax_collection, marginal_economic_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in geographic jurisdiction with mandatory tax withholding at source. Cannot exit taxation without exiting employment or jurisdiction. Suppression high: withholding removes agency; legal penalties prevent evasion; financial dependence prevents relocation. No coordination benefit perceived — taxation appears as pure extraction.
constraint_indexing:constraint_classification(provincial_tax_collection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Constrained by jurisdiction-specific licensing, customer base, and relocation costs. Experiences genuine coordination: tax-funded infrastructure (roads, courts, utilities) enables commerce. Also experiences asymmetric extraction: compliance burden disproportionate to business size; audit risk; quarterly filing requirements. Both benefits and costs are real.
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROVINCIAL GOVERNMENT (ROPE) — Experiences tax collection as coordination mechanism. Funding public goods (education, healthcare, infrastructure) is the solving of a collective action problem. Can arbitrage: fiscal federalism allows inter-provincial tax rate variation. Net beneficiary position: extraction flows toward government through legitimate institutional authority.
constraint_indexing:constraint_classification(provincial_tax_collection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MOBILE CAPITAL SECTOR (TANGLED ROPE) — Organized corporate actors with exit capacity (can relocate, arbitrage between jurisdictions). Benefit from public infrastructure and rule of law. Also extract value: can demand tax breaks, subsidies, preferential treatment. Both coordination and extraction present. Mobile exit options reduce suppression but increase agency-based extraction mechanisms.
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX ADMINISTRATION BUREAUCRACY (PITON) — Maintains elaborate compliance theater (forms, filings, audits, notices) that persists through institutional inertia. Much of the apparatus is performative rather than functionally necessary: digital systems could replace paper processes; pre-filled returns could replace manual filing. Theater ratio high because the bureaucracy itself perpetuates complexity as justification for its existence. Degraded function maintained by inertia, not necessity.
constraint_indexing:constraint_classification(provincial_tax_collection, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: WEALTH HOLDER (TANGLED ROPE) — Powerful agents with truly mobile exit (can relocate, hold assets offshore, arbitrage jurisdictions). Benefit from property-rights protection and rule of law that provincial government provides. Also possess extraction capacity: can demand special treatment, exploit tax planning strategies, transfer liability to wage earners through corporate structures. Both genuine coordination function and asymmetric extraction coexist.
constraint_indexing:constraint_classification(provincial_tax_collection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, taxation is inherent to any state: collective governance requires revenue sources; taxation is the irreducible mechanism. State formation and taxation are coextensive. However, this perspective naturalizes what is a contingent institutional choice — the form, rate, and mechanism of taxation are highly variable and contestable. The engine will identify this as a false summit: the natural law framing conceals political economy.
constraint_indexing:constraint_classification(provincial_tax_collection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_tax_collection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(provincial_tax_collection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(provincial_tax_collection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_tax_collection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(provincial_tax_collection, TR),
    TR >= 0.70.

:- end_tests(provincial_tax_collection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The value reflects that provincial tax systems genuinely solve public-goods coordination problems (roads, courts, defense, education) while simultaneously extracting through asymmetric burden distribution. Progressive vs regressive rate structures, compliance complexity, audit targeting, and preferential treatment for organized actors all contribute extraction mechanisms. The value is not higher (snare territory) because coordination functions are genuinely provided — citizens receive services for their taxes, not zero-sum extraction. The value is not lower (rope territory) because extraction mechanisms are substantial: burden distribution is asymmetric, compliance theater adds deadweight loss, and organized actors obtain preferential treatment. Suppression (0.68): Moderate-high. Structural suppression includes withholding at source (removes agency from wage earners), legal penalties for non-compliance (deters exit through evasion), territorial residence (penalizes relocation), and information asymmetry (government has superior audit capacity). Perceptual suppression includes legitimacy (many agents internalize authority as natural), identity fusion (provincial identity may constitute self-concept), and normalized extraction (long institutional history makes extraction invisible). Theater ratio (0.55): Moderate. Tax administration includes genuine functions (revenue collection, audit) and performative functions (elaborate forms, compliance theater, procedural complexity). Digital systems could reduce theater substantially, but bureaucratic inertia and employment maintenance incentives perpetuate complexity. The theater ratio has increased over the measurement interval as compliance procedures have elaborated while actual collection efficiency has not improved proportionally.
 *
 * PERSPECTIVAL GAP:
 *   Wage earner → Snare: trapped exit + victim status = maximum experienced extraction. Small business → Tangled Rope: constrained exit + mixed (beneficiary of infrastructure, victim of burden) = moderate extraction with coordination benefit. Government → Rope: arbitrage exit + beneficiary status = net-negative extraction (they are the receiver). Organized capital → Tangled Rope: mobile exit + extraction capacity + coordination benefits = moderate chi, organized agent power. This gap is structural: the same tax revenue that the government experiences as legitimately collected public funding, the wage earner experiences as extraction with no coordination benefit. Neither is wrong; they occupy opposite positions in the value flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status combined with exit options. Wage earners: d ≈ 0.95 (trapped victim) → high f(d) ≈ 1.42 → high experienced extraction. Small business: d ≈ 0.55 (constrained, mixed) → moderate f(d) ≈ 0.75 → moderate extraction. Government: d ≈ 0.05 (beneficiary with arbitrage) → low f(d) ≈ -0.12 → negative/institutional extraction (receiver). Organized capital: d ≈ 0.50 (mobile, mixed) → moderate f(d) ≈ 0.65 → moderate extraction. Wealth holder: d ≈ 0.30 (beneficiary with mobile exit) → low f(d) ≈ 0.15 → low extraction despite power. Bureaucracy: d ≈ 0.08 (beneficiary with arbitrage) → low f(d) ≈ -0.10 → institutional position. The spatial scope modifier σ(S) = 1.0 for regional scope in most perspectives; shifts to 1.1 continental or 1.2 global for analysis of inter-jurisdictional arbitrage. No directionality overrides are needed — the structural data produces accurate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that extractiveness > 0.46 is compatible with genuine coordination (tangled rope, not pure snare) BECAUSE the system genuinely provides public goods. The tension is not 'is this coordination or extraction?' but 'what is the proportion and distribution?' A snare would be taxation with no public goods provision (extraction pure and simple). A rope would be coordination with symmetric benefit distribution. Provincial tax collection is tangled: real coordination + asymmetric distribution + extraction mechanisms. The mandatrophy dissolves when the analytical observer recognizes that moderate extractiveness is consistent with genuine public-goods coordination and does not require either denying the coordination function or denying the extraction mechanisms. Both are present. The classification as tangled_rope (not snare) is justified by the presence of coordinated public goods; the high suppression (0.68) reflects the asymmetric burden. The false summit in the mountain perspective is the claim that taxation is inherent and immutable to statecraft — the form, rate, and mechanism are highly contestable political choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_goods_coordination_magnitude,
    'What proportion of collected tax revenue actually funds genuine public goods coordination versus extraction, rent-seeking, and administrative overhead?',
    'Comparative public finance analysis: decompose provincial budgets into genuine coordination functions (core infrastructure, courts, defense), redistributive functions (healthcare, education), and rent-seeking (subsidies, preferential contracts, bureaucratic expansion)',
    'If coordination > 60%: classification shifts toward Rope for more perspectives, suppression decreases. If coordination < 40%: classification shifts toward Snare/pure extraction, suppression increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_goods_coordination_magnitude, empirical, 'Proportion of revenue genuinely coordinating public goods').

omega_variable(
    suppression_structural_vs_perceived,
    'Is high suppression (0.68) structural (legal barriers, physical coercion, economic dependency) or perceptual (internalized compliance, normalized authority)?',
    'Post-exit analysis: measure suppression persistence after agents relocate to lower-tax jurisdictions; survey data on compliance motivation (fear vs legitimacy vs identity with state); historical comparison with periods of perceived legitimacy vs delegitimization',
    'If primarily structural: suppression remains stable across contexts. If primarily perceptual: suppression drops sharply when legitimacy erodes, revealing identity_locked mechanism rather than trapped mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_perceived, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    exit_capacity_heterogeneity,
    'How binary is the exit-options spectrum? Do most taxpayers occupy discrete categories (trapped vs mobile) or a continuous gradient?',
    'Population segmentation analysis: measure relocation costs, asset mobility, jurisdiction arbitrage feasibility for decile-distributed income levels; identify tipping points where exit becomes materially feasible',
    'If binary: perspectives are correctly classified as trapped/mobile/arbitrage. If continuous: single perspectives may conceal heterogeneous experiences within power atoms; some ''powerless'' agents may have constrained-level exit that isn''t captured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_capacity_heterogeneity, empirical, 'Whether exit capacity is distributed continuously or discretely').

omega_variable(
    legitimacy_base_identity_fusion,
    'To what degree do taxpayers internalize provincial authority as identity-constituting versus perceiving it as an external constraint?',
    'Identity-frame analysis: survey and ethnographic data on self-concept alignment with state/province; measure willingness to relocate after legitimacy shocks (policy reversals, scandal, institutional failure); compare identity fusion between natural-born residents vs recent migrants',
    'If high identity fusion: identity_locked perspective is appropriate for some agents even with constrained-level exit. If low fusion: trapped/constrained distinction captures exit options accurately without identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_base_identity_fusion, empirical, 'Degree of identity fusion with provincial authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_tax_collection, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tax_tr_t0, provincial_tax_collection, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prov_tax_tr_t2, provincial_tax_collection, theater_ratio, 2, 0.48).
narrative_ontology:measurement(prov_tax_tr_t4, provincial_tax_collection, theater_ratio, 4, 0.53).
narrative_ontology:measurement(prov_tax_tr_t6, provincial_tax_collection, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(prov_tax_be_t0, provincial_tax_collection, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prov_tax_be_t2, provincial_tax_collection, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(prov_tax_be_t4, provincial_tax_collection, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(prov_tax_be_t6, provincial_tax_collection, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_tax_collection, resource_allocation).
narrative_ontology:affects_constraint(provincial_tax_collection, fiscal_federalism).
narrative_ontology:affects_constraint(provincial_tax_collection, tax_competition_between_jurisdictions).
narrative_ontology:affects_constraint(provincial_tax_collection, welfare_state_sustainability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
