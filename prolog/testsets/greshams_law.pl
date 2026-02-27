% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_greshams_law, []).

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
 *   constraint_id: greshams_law
 *   human_readable: Gresham's Law ("Bad money drives out good")
 *   domain: economic/monetary
 *
 * SUMMARY:
 *   Gresham's Law describes a structural constraint emerging from the
 *   interaction of commodity-based money, differential intrinsic value, and
 *   rational agent behavior. When two forms of money have the same nominal
 *   (face) value but different intrinsic values — such as coins with high vs.
 *   low silver content — rational actors will spend the debased coins and
 *   hoard the good ones. This drives good money out of circulation, leaving
 *   only bad money in active use. The constraint exhibits a full spectrum of
 *   perspectival classifications depending on the observer's structural
 *   position relative to the extraction and coordination flows. Savers
 *   experience pure extraction (snare); monetary authorities experience
 *   coordination benefit (rope); merchants experience a mixed system (tangled
 *   rope); historical banking elites see a degraded mechanism (piton);
 *   organized groups building alternative monetary systems see a temporary
 *   problem with a structured exit (scaffold); and civilizational observers
 *   risk naturalizing what is a choice about monetary architecture as an
 *   inevitability (false mountain). The constraint is primarily a feature of
 *   commodity-money systems; its force has declined dramatically since the
 *   transition to fiat currency and centralized monetary standards in the
 *   20th century. However, the law re-emerges in cryptocurrency and
 *   multi-asset monetary environments, suggesting the underlying mechanism is
 *   not extinct but dormant.
 *
 * KEY AGENTS:
 *   - Savers Holding Good Money: Primary victims (powerless/trapped) — forced to watch accumulated wealth disappear from circulation as debased coins replace them
 *   - Monetary Authorities: Primary beneficiaries (institutional/arbitrage) — profit from seigniorage and use debasement as hidden taxation mechanism
 *   - Common Merchants: Secondary actors (moderate/constrained) — benefit from money's circulation function but suffer extraction when forced to accept bad coins at face value
 *   - Banking Elite: Tertiary actors (powerful/arbitrage) — historically extracted advanced knowledge of debasement; advantage now largely degraded
 *   - Commodity Money Replacement Regime: Organized reformers (organized/constrained) — building fiat currency and central banking systems that structurally eliminate the distinction between good and bad money
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing monetary architecture choices as economic inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greshams_law, 0.38).
domain_priors:suppression_score(greshams_law, 0.45).
domain_priors:theater_ratio(greshams_law, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(greshams_law, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(greshams_law, tangled_rope).
narrative_ontology:human_readable(greshams_law, "Gresham's Law (\"Bad money drives out good\")").
narrative_ontology:topic_domain(greshams_law, "economic/monetary").

domain_priors:requires_active_enforcement(greshams_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(greshams_law, debasers_and_counterfeiters).
narrative_ontology:constraint_beneficiary(greshams_law, monetary_authorities_seeking_seigniorage).
narrative_ontology:constraint_victim(greshams_law, savers_holding_good_money).
narrative_ontology:constraint_victim(greshams_law, price_stability_and_trust_in_currency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAVER HOLDING GOOD MONEY (SNARE) — Cannot exit the currency system without extreme cost. Forced to watch their carefully accumulated good money disappear from circulation as debased coins drive it out. Trapped between hoarding (losing purchasing power to inflation) and spending (receiving bad money in return). Maximum extraction: the constraint directly transfers wealth from savers to debasers.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MONETARY AUTHORITY (ROPE) — Benefits from seigniorage (profit from issuing debased coinage). Experiences the constraint as a coordination solution: debasement allows authorities to fund operations without explicit taxation. The mechanism coordinates money supply adjustment with revenue needs. Arbitrage option: can always debase further or switch to new currency. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(greshams_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMON MERCHANT (TANGLED ROPE) — Constrained by the currency system but not completely trapped. Benefits from the circulation of money itself (enables trade coordination) but suffers extraction when forced to accept bad money at face value while good money vanishes. Must develop alternative mechanisms (weight-based pricing, foreign coin preference) to mitigate losses. Mixed experience: coordination benefit from monetary system plus asymmetric extraction through debasement.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BANKING ELITE (PITON) — Historically could profit from debasement through advance knowledge of coin quality degradation. Over time, this advantage has largely degraded as debasement became expected, priced in, and eventually superseded by fiat currency. The constraint persists in financial memory and regulatory frameworks but has lost functional extraction force. Theatrical observation of historical patterns without contemporary extraction mechanism.
constraint_indexing:constraint_classification(greshams_law, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: COMMODITY MONEY REPLACEMENT (SCAFFOLD) — The emergence of fiat currency systems, central banking standards, and modern monetary policy represents a structured exit from Gresham's Law as originally formulated. By removing the distinction between 'good' and 'bad' commodity money (both are now abstract digital ledger entries with uniform face value), the constraint's extraction mechanism is dismantled. However, this creates new constraints around inflation targeting and trust in central authorities. The transition horizon is civilizational; suppression is being systematically reduced through monetary reform.
constraint_indexing:constraint_classification(greshams_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, some version of Gresham's Law appears as an inevitable feature of commodity-based monetary systems: if two goods have the same nominal value but different intrinsic utility, rational actors will preserve the more valuable one. This seems like a natural law of exchange. However, structural data reveals this is NOT a natural law but a contingent feature of commodity money systems. Fiat currency, legal-tender laws, and standardized measure eliminate the distinction. The mountain classification is a false summit — it naturalizes a choice about monetary architecture.
constraint_indexing:constraint_classification(greshams_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: MODERN CRYPTOCURRENCY COMMUNITY (TANGLED ROPE) — Gresham's Law re-emerges in cryptocurrency ecosystems where different coins/tokens have different intrinsic utility but face similar acceptance. 'Good' cryptocurrencies with genuine utility (low inflation, secure consensus) get hoarded; 'bad' coins (high inflation, questionable security) circulate. This is a pure instantiation of the original law in a new medium. Communities experience both coordination benefit (alternative monetary systems enable peer-to-peer exchange) and extraction cost (hoarding dynamics reduce circulation efficiency). Organized agents with mobile exit options (can switch coins or fiat) experience moderate extraction.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(greshams_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(greshams_law, TR),
    TR >= 0.70.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Gresham's Law represents a real transfer of wealth from savers to debasers, but the extraction is not maximal because: (1) savers can partially mitigate through hoarding, creating a distributed rather than total loss; (2) the mechanism operates through collective action (rational individual behavior) rather than overt coercion; (3) modern monetary systems have largely eliminated the conditions that enable the extraction. The trajectory shows declining extractiveness over the measurement interval (0.52 → 0.38) reflecting the historical transition from commodity to fiat money. Suppression (0.45): Moderate. Savers face significant barriers to exit: they cannot easily abandon currency without extreme transaction costs, and hoarding reduces purchasing power through inflation. However, suppression is not absolute — savers can develop workarounds (weight-based pricing, foreign coins, trade barter), and over centuries, they drove the replacement of commodity money with fiat systems. Theater ratio (0.35): Low-moderate. The mechanism is functionally real (rational actors do hoard good money when facing debased coins), not primarily performative. However, some theatrical elements exist: the 'law' is often cited in ways that overstate its contemporary relevance, and post-fiat discussions of Gresham's Law often serve to legitimize particular monetary policy positions rather than describe observed behavior.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same economic phenomenon is experienced completely differently across structural positions. The saver trapped in a debasement cycle experiences pure extraction (snare) — their accumulated good money is systematically replaced by bad money, and they have no exit. The monetary authority experiences the same phenomenon as coordination (rope) — debasement solves the problem of funding government without explicit taxation, and it coordinates monetary supply with revenue needs. The merchant sees a mixed system (tangled rope) — they depend on money for commerce coordination but are forced to accept bad coins, creating friction and adjustment costs. The analytical observer from a civilizational distance risks seeing an economic inevitability or natural law (mountain), naturalizing what is actually a choice about monetary architecture. The gap reveals that 'bad money drives out good' is not an inexorable law of nature but a structural feature of specific monetary systems that can be redesigned. Modern fiat currency systems have largely eliminated the distinction between 'good' and 'bad' money by standardizing all currency to uniform nominal value backed by legal tender laws, not commodity intrinsic value.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes the structural position of each agent relative to the extraction flow. Savers holding good money have d ≈ 0.90 (near-total target): they lose wealth directly to the hoarding/circulation dynamic. Monetary authorities have d ≈ 0.05 (near-total beneficiary): they gain seigniorage and hidden taxation directly. Merchants have d ≈ 0.50 (symmetric): they benefit from money's circulation function but suffer losses when forced to accept bad coins. The sigmoid f(d) transforms these into effective power modifiers. Savers and authorities are at opposite extremes; merchants are in the middle. The piton perspective (banking elite) has derived d around 0.35 but high theater (0.70+) because the extraction mechanism has largely atrophied — the advantage persists in memory and institution but not in active operation. The mountain perspective's high d combined with analytical power and universal scope would suggest massive extraction, but this is a false summit: the 'natural law' framing misses that the constraint is contingent on commodity-based monetary architecture.
 *
 * MANDATROPHY ANALYSIS:
 *   Gresham's Law resolves mandatrophy by showing that the constraint is NOT a single economic phenomenon classified uniformly, but rather a family of distinct structural mechanisms observed across different monetary architectures. The medieval/early-modern instantiation (commodity money with differential intrinsic value) is a genuine tangled rope: it coordinates commerce through monetary exchange while extracting wealth from savers through debasement. The modern fiat currency incarnation is NOT a mountain (inevitable natural law) but a deliberate architectural choice to eliminate the conditions that enable the law. The cryptocurrency re-emergence is a new tangled rope in a different medium. The false summit at the civilizational/analytical perspective reveals the core insight: Gresham's Law is often cited as if it were a natural law, but it is actually a statement about rational behavior under specific constraints (commodity money, differential intrinsic value, legal tender enforcement). Remove those constraints, and the law no longer operates. The mandatrophy resolves by decomposing 'Gresham's Law' into multiple constraint stories: (1) commodity_money_extraction (0.52, snare from saver perspective), (2) fiat_currency_monetary_coordination (0.10, rope from all perspectives — the constraint largely disappears), (3) cryptocurrency_circulation_dynamics (0.35, tangled rope in new medium). The network connects them as a family with affects_constraints relationships, showing how the transition from commodity to fiat was a structural redesign that dismantled the extraction mechanism while preserving the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commodity_vs_fiat_boundary,
    'Is Gresham''s Law a structural feature of commodity-based money systems only, or does it generalize to any multi-asset monetary environment with differential trust/utility?',
    'Historical analysis of fiat currency periods with competing monetary assets (e.g., Bretton Woods, cryptocurrency emergence); comparative study of legal-tender law enforcement and money substitution patterns',
    'If fiat-only: the law is nearly extinct in modern economies and should be reclassified as historical piton. If generalizable: the law persists under new forms (cryptocurrency, parallel currencies, store-of-value bifurcation) and remains a structural constraint on monetary design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commodity_vs_fiat_boundary, empirical, 'Whether Gresham''s Law is specific to commodity money or generalizes to fiat systems').

omega_variable(
    hoarding_vs_velocity_tradeoff,
    'Does the empirical observation of ''bad money driving out good'' reflect rational hoarding behavior by savers, or does it reflect merchant preference for low-valued coins in everyday transactions, with high-valued coins retained in savings independently?',
    'Numismatic evidence from medieval coin hoards; monetary velocity data stratified by coin type during debasement periods; experimental economics replication of the mechanism',
    'If hoarding-driven: the constraint is about wealth extraction from savers and represents high experienced suppression. If velocity-driven: the constraint is primarily about commerce friction and represents lower suppression (merchants adapt quickly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hoarding_vs_velocity_tradeoff, empirical, 'Whether bad money drives out good through hoarding or through commerce velocity differentiation').

omega_variable(
    legal_tender_enforcement_sufficiency,
    'Can modern legal-tender laws and fiat currency standards definitively prevent Gresham''s Law from operating, or do they merely suppress its manifestation while leaving the underlying incentive structure intact?',
    'Study of hyperinflation episodes and currency substitution despite legal-tender laws; analysis of cryptocurrency adoption in fiat-currency regimes; formal model of legal enforcement costs vs. evasion incentives',
    'If laws are sufficient: Gresham''s Law is a resolved constraint in modern economies (piton or degraded rope). If underlying structure persists: the law remains latent and re-emerges whenever monetary architecture is contested or trust declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_tender_enforcement_sufficiency, conceptual, 'Whether legal-tender enforcement sufficiently resolves Gresham''s Law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(greshams_law, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gresh_tr_t0, greshams_law, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gresh_tr_t3, greshams_law, theater_ratio, 3, 0.3).
narrative_ontology:measurement(gresh_tr_t6, greshams_law, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(gresh_be_t0, greshams_law, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gresh_be_t3, greshams_law, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(gresh_be_t6, greshams_law, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(greshams_law, resource_allocation).
narrative_ontology:affects_constraint(greshams_law, debasement_fiscal_extraction).
narrative_ontology:affects_constraint(greshams_law, seigniorage_as_hidden_tax).
narrative_ontology:affects_constraint(greshams_law, cryptocurrency_velocity_dynamics).

% DUAL FORMULATION NOTE:
% Gresham's Law decomposes into multiple constraint stories depending on the monetary architecture. The commodity-money version (0.38-0.52 extractiveness) is historically primary and describes the circulation suppression mechanism. The fiat-currency version (0.05-0.10 extractiveness) represents the constraint largely eliminated by architectural redesign. The cryptocurrency version (0.35-0.40 extractiveness) shows the mechanism re-emerging in multi-asset environments. All three are related through network.affects_constraints; each has its own ε and perspectives reflecting different structural positions within its respective monetary system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(greshams_law, institutional, 0.05).
constraint_indexing:directionality_override(greshams_law, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
