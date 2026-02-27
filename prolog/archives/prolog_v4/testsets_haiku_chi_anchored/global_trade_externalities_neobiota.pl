% ============================================================================
% CONSTRAINT STORY: global_trade_externalities_neobiota
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_trade_externalities_neobiota, []).

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
 *   constraint_id: global_trade_externalities_neobiota
 *   human_readable: Global Trade's Externalization of Neobiota Costs
 *   domain: economic/environmental
 *
 * SUMMARY:
 *   The global trade system since the 1980s has dramatically accelerated the
 *   movement of goods across biogeographic boundaries, but the institutional
 *   infrastructure for pricing and controlling the spread of invasive alien
 *   species (neobiota) has not kept pace. The constraint exhibits a
 *   fundamental asymmetry: traders and exporters benefit from low-friction
 *   movement of containers and commodities, while the ecological and economic
 *   costs of invasive species establishment are borne by recipient regions
 *   and the global commons. This is a classic case of institutional
 *   coordination (WTO SPS agreement) failing to capture externalities. The
 *   constraint is not purely extractive because the trade system does provide
 *   genuine coordination benefits (reduced transaction costs, market access,
 *   economies of scale), but these benefits accrue disproportionately to
 *   traders while costs concentrate on invaded regions, island ecosystems,
 *   and agricultural producers with no capacity to exit. The theater_ratio
 *   (0.54) reflects moderate performative content: the SPS apparatus produces
 *   extensive documentation and expert committee work, but the actual
 *   capacity to prevent invasive arrivals remains limited, particularly for
 *   cryptic species and slow-moving biological vectors. Over the 44-year
 *   interval, extractiveness has more than tripled (0.15 → 0.58) as global
 *   container traffic and agricultural commodity volumes have exploded, but
 *   quarantine investment has grown more slowly, causing the extraction
 *   mechanism to harden.
 *
 * KEY AGENTS:
 *   - International commodity traders and shipping operators: Primary beneficiary (institutional/arbitrage) — capture profits from rapid, low-friction container movement across borders without bearing neobiota costs
 *   - Agricultural producers in invaded regions: Primary victim (powerless/trapped) — face crop loss, property value collapse, and mandatory eradication costs with no capacity to prevent invasive arrivals
 *   - Native ecosystem services and island biotas: Primary victim (powerless/trapped) — abstract collective goods that cannot organize; face permanent ecological damage and service collapse
 *   - Quarantine and biosecurity agencies: Secondary institutional actor (institutional/constrained) — coordinating trade but unable to enforce full biotic border without diplomatic and economic cost
 *   - Export-oriented agricultural sectors: Mixed actor (powerful/mobile) — benefit from low-cost shipping but face increasing regulatory pressure and brand risk in destination markets
 *   - WTO trade regime apparatus: Institutional guardian (institutional/constrained) — maintains SPS coordination framework but its function for invasive species control has atrophied relative to trade volume growth
 *   - Regional biotic border coalitions: Organized emerging actor (organized/constrained) — building alternative quarantine and coexistence management with defined lifecycle
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing neobiota externalization as inherent to global trade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_trade_externalities_neobiota, 0.58).
domain_priors:suppression_score(global_trade_externalities_neobiota, 0.68).
domain_priors:theater_ratio(global_trade_externalities_neobiota, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_trade_externalities_neobiota, tangled_rope).
narrative_ontology:human_readable(global_trade_externalities_neobiota, "Global Trade's Externalization of Neobiota Costs").
narrative_ontology:topic_domain(global_trade_externalities_neobiota, "economic/environmental").

domain_priors:requires_active_enforcement(global_trade_externalities_neobiota).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, international_commodity_traders).
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, shipping_container_operators).
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, export_oriented_agricultural_sectors).
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, low_cost_import_retailers).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, agricultural_producers_invaded_regions).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, native_ecosystem_services).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, island_biotas).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, freshwater_ecologies).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, developed_nations_quarantine_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INVADED AGRICULTURAL PRODUCER (SNARE) — Farmers in recipient regions (Pacific island nations, African savannas, Mediterranean basins) face invasive species establishment after trade goods arrive. No exit option: cannot prevent arrival or retroactively uninvade. Bears full ecological and economic cost of crop loss, property value collapse, and mandatory eradication attempts. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95. Maximum extraction.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIVE ECOSYSTEM SERVICES (SNARE) — Pollination, pest control, soil formation, water filtration, and nutrient cycling provided by native species are systematically destroyed by neobiota. No agent represents these collective goods; they cannot organize or exit. Permanent structural damage (invasive establishment is quasi-irreversible). d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Pure extraction from the ecological commons.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: QUARANTINE AND BIOSECURITY AGENCIES (TANGLED ROPE) — USDA, APHIS, Australian Quarantine, EU plant health agencies coordinate international trade through phytosanitary certification (SPS agreement via WTO). They benefit from trade coordination (information sharing, harmonized standards, logistics efficiency). But they are also constrained: unable to fully enforce biotic borders without crushing trade, facing asymmetric litigation risk when inspections block imports, and absorbing escalating containment costs. Active enforcement required; constrained exit (cannot withdraw from WTO SPS framework without diplomatic cost). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44. Hybrid coordination and extraction.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL COMMODITY TRADERS (ROPE) — Shipping companies, container operators, bulk commodity brokers coordinate global supply chains. The constraint (lack of neobiota pricing) enables their profit margin: they can load containers without quarantine delays, pass-through costs to recipient regions. They experience the constraint as pure coordination: efficient logistics networks. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary; constraint looks like functional coordination from their position.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXPORT-ORIENTED AGRICULTURAL SECTOR (TANGLED ROPE) — Large-scale commodity exporters (soybeans, palm oil, grain) benefit from low-cost shipping and minimal phytosanitary overhead. But they are also constrained: facing increasing pressure from destination-country invasive species regulations, litigation from ecological damage, and brand risk from 'vector' association. Mobile exit options (can shift sourcing or sourcing regions) but significant regional regulatory constraints. d≈0.45, f(d)≈0.45, σ=1.1 → χ≈0.28. Moderate extraction; both beneficiary and victim by region.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: WTO TRADE REGIME APPARATUS (PITON) — The WTO SPS (Sanitary and Phytosanitary) agreement was designed to coordinate trade while preventing protectionist disguises as health measures. The apparatus maintains elaborate verification, expert committees, and dispute settlement. But its primary function — pricing externalities and allocating invasive species costs — has atrophied: the dispute panel rarely penalizes countries for inadequate quarantine, and the SPS agreement provides loopholes for countries genuinely unable to police borders. Theater ratio = 0.54 (moderate): the agreement produces extensive documentation, committee meetings, and certification rituals, but limited actual biotic control. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.03. Institutional inertia — the apparatus persists because it coordinated trade in a previous era, not because it solves the neobiota constraint.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EMERGING BIOTIC BORDER COALITIONS (SCAFFOLD) — Regional organizations (Pacific Island Forum, African Union, EU biosecurity networks) and scientific communities (IUCN, invasive species networks) are building coordinated border control with a defined sunset: high-cost early detection and eradication give way to long-term coexistence management as invasive populations stabilize. These coalitions experience the constraint as temporary and solvable. d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.11. Low effective extraction because the coalition sees a transition path; theater_ratio is also lower (~0.30) because the focus is on actual biocontrol, not ritual certification.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, global trade networks always increase biotic mixing. Preventing all transboundary species transfer would require shutting down trade (thermodynamically infeasible). The constraint appears as an immutable consequence of interconnection and transport networks — a natural law of global commerce. However, structural data (ε=0.58, suppression=0.68) contradicts mountain classification. The engine detects false summit: neobiota externalization is not inherent to trade but to the failure to price it in (contingent institutional choice), so this perspective naturalizes what is actually a tangled rope.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_trade_externalities_neobiota_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_trade_externalities_neobiota, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_trade_externalities_neobiota, TR),
    TR >= 0.70.

:- end_tests(global_trade_externalities_neobiota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The global trade system captures substantial economic rent (profits from low-cost shipping and minimal quarantine overhead) while externalizing the majority of invasive species costs to recipient regions and ecosystem services. The value reflects that the extraction is real but not total — traders do pay some quarantine fees, and the benefit of trade (lower consumer prices, market access for exporters) is genuine. Suppression (0.68): High. Multiple barriers prevent invasion victims from exiting or organizing: geographical immobility of farms and native species, lack of retroactive uninvasion technology, long lags between invasion arrival and detection (cryptic establishment), and international legal frameworks (WTO SPS) that prioritize trade freedom over biotic border enforcement. Theater ratio (0.54): Moderate. The WTO SPS apparatus produces extensive documentation, expert committee meetings, and certification protocols. However, these are moderately performative: inspections catch obvious vectors but miss cryptic ones, expert committees produce guidelines that national agencies often under-implement, and the actual rate of invasive species prevention is unknown (detectability of cryptic vectors is an unresolved omega). The theater has increased from 0.35 to 0.54 as the apparatus has added committee layers without proportional increases in quarantine effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power scales and structural positions. The invaded agricultural producer sees pure extraction (Snare) — they bear full costs with no benefit from trade. Native ecosystem services see pure extraction (Snare) — permanent damage with no agents representing their interests. Quarantine agencies see mixed coordination and extraction (Tangled Rope) — they benefit from SPS coordination and information sharing but are constrained by the impossibility of perfect biotic closure. Commodity traders see pure coordination (Rope) — their experience is efficient logistics networks with low friction. Export-oriented agriculture sees mixed extraction (Tangled Rope) — both beneficiary (cheap shipping) and victim (regulatory pressure, invasion feedback). The WTO apparatus sees degraded ritual (Piton) — the SPS framework was designed to balance trade and biotic control, but its primary function has atrophied as trade volume outpaced quarantine capacity. Emerging biotic border coalitions see a temporary problem with a sunset (Scaffold) — regional early detection and eradication gives way to managed coexistence. The civilizational analytical observer risks seeing immutable natural law (Mountain) — 'global trade always mixes biotas' — but the structural data (ε=0.58, high suppression but not maximum, theater ratio moderate) reveals this as a false summit: the problem is institutional failure to price externalities, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Invaded agricultural producer: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Native ecosystem services: Victim + trapped + powerless + civilizational → d≈0.95, f(d)≈1.42. Maximum extraction (irreversible damage, no retroactive control). Quarantine agencies: Both beneficiary (SPS coordination) and victim (impossible enforcement task); constrained exit + institutional power → d≈0.55, f(d)≈0.75. Moderate extraction; constrained by trade framework. Commodity traders: Beneficiary + arbitrage + institutional → d≈0.08, f(d)≈-0.08. Negative extraction (net beneficiary). Export-oriented agriculture: Beneficiary (shipping costs) + victim (regulatory pressure, invasion feedback) + mobile exit + powerful → d≈0.45, f(d)≈0.45. Moderate mixed extraction; can shift regions/suppliers. WTO apparatus: Institutional + constrained + inert function → d≈0.15, f(d)≈0.05. Low effective extraction (piton classification from theater gate). Biotic border coalitions: Organized + constrained + generational view + sunset → d≈0.38, f(d)≈0.38. Low extraction (coalition has agency and sees a path). Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit detector flags the mountain classification as perspectival naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint's classification as tangled_rope (claimed) is supported by structural data but depends critically on omega variables that are not yet resolved. The ambiguity: Is the constraint primarily a coordination mechanism (WTO SPS agreement providing genuine benefits to all parties) with unpriced externalities (tangled rope), or is it a pure extraction mechanism where the 'coordination benefits' are illusory and serve only to legitimize theft from the commons (snare)? The resolution hinges on three unresolved questions: (1) origin-state liability assignment (preference-class omega) — currently unassigned, allowing traders to externalize costs; (2) detectability of cryptic vectors (empirical omega) — if <50% detectable, suppression is structural and extraction mechanism hardens; (3) long-term coexistence productivity (empirical omega) — if invasions cause permanent collapse, the constraint is civilizational-scale extraction rather than temporary coordination failure. If omegas resolve to: origin liability assigned + high detectability + stable coexistence, the constraint hardens toward scaffold (temporary problem with sunset). If omegas resolve to: origin liability unassigned + low detectability + permanent collapse, the constraint hardens toward snare (pure extraction with no escape). The tangled_rope classification (ε=0.58, χ=0.44-0.95 across perspectives) reflects the present state of uncertainty. Mandatrophy is currently unresolved; high-confidence resolution would require empirical data from 2-3 omega classes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_state_liability_assignment,
    'Should exporting countries bear financial liability for invasive species that escape their shipments, and if so, how is causation established in complex polyglot invasion pathways?',
    'International legal precedent analysis; DNA/genomic tracing of invasive populations to source shipments; cost-benefit analysis of origin-state liability vs destination-state quarantine investment',
    'If origin-state liability: extraction shifts upstream to traders/exporters (snare from their perspective shifts to tangled rope). If destination-state bears cost: current snare for invaded regions persists, but incentivizes regional coalitions (scaffold). Liability assignment fundamentally changes χ directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_state_liability_assignment, preference, 'Origin-state liability for invasive species escape').

omega_variable(
    detectability_of_cryptic_vectors,
    'What fraction of neobiota pathways are scientifically detectable vs undetectable in real time (slow-living species, seed dormancy, cryptic invertebrates in soil)?',
    'Taxonomic audit of known invasions: trace-back to source and pathway identification; comparison of detected vs estimated total pathways; analysis of detection lag between arrival and establishment',
    'If >70% detectability: suppression is behavioral not physical (quarantine agencies can catch vectors with investment) → classification shifts to rope/tangled rope from snare. If <50% detectability: suppression is structural (many arrivals are undetectable) → snare classification hardens; even high-investment quarantine fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(detectability_of_cryptic_vectors, empirical, 'Fraction of neobiota vectors scientifically detectable').

omega_variable(
    coexistence_long_term_productivity,
    'Do invaded agricultural and ecological systems reach stable coexistence with reduced productivity, or does invasive-driven ecosystem collapse create permanent economic loss that compounds over generations?',
    'Long-term productivity tracking in invaded regions (20+ year agricultural yield data, ecosystem service valuation pre/post invasion); comparison of managed coexistence vs unmanaged invasion trajectories; genetic adaptation in crops to invasive presence',
    'If coexistence + partial recovery: extraction is temporary (scaffold perspective hardens). If permanent collapse: extraction is civilizational-scale and irreversible (snare deepens across time horizons). This determines whether the constraint has a sunset or only degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_long_term_productivity, empirical, 'Long-term productivity under invaded conditions').

omega_variable(
    trade_volume_elasticity_to_quarantine_cost,
    'How much would international trade volume contract if quarantine costs were fully internalized (added to container shipping costs)?',
    'Economic modeling: trade elasticity to shipping cost increases; historical precedent from sudden tariff increases; scenario analysis of full-cost pricing regimes',
    'If elasticity > 0.8 (high): full cost pricing would collapse trade significantly, making tangled rope classification stable (suppression would remain high, but χ shifts as beneficiaries have less arbitrage room). If elasticity < 0.3 (low): traders can absorb costs and maintain margins — snare persists because extraction capacity remains even under higher prices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trade_volume_elasticity_to_quarantine_cost, empirical, 'Trade volume sensitivity to quarantine cost internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_trade_externalities_neobiota, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neobiota_tr_t1980, global_trade_externalities_neobiota, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(neobiota_tr_t2000, global_trade_externalities_neobiota, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(neobiota_tr_t2024, global_trade_externalities_neobiota, theater_ratio, 2024, 0.54).

% Extraction over time
narrative_ontology:measurement(neobiota_be_t1980, global_trade_externalities_neobiota, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(neobiota_be_t2000, global_trade_externalities_neobiota, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(neobiota_be_t2024, global_trade_externalities_neobiota, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_trade_externalities_neobiota, global_infrastructure).
narrative_ontology:boltzmann_floor_override(global_trade_externalities_neobiota, 0.25).
narrative_ontology:affects_constraint(global_trade_externalities_neobiota, international_shipping_biosecurity).
narrative_ontology:affects_constraint(global_trade_externalities_neobiota, agricultural_supply_chain_pathogen_spread).
narrative_ontology:affects_constraint(global_trade_externalities_neobiota, island_ecosystem_isolation_loss).
narrative_ontology:affects_constraint(global_trade_externalities_neobiota, wto_sps_agreement_effectiveness).

% DUAL FORMULATION NOTE:
% The global trade neobiota constraint decomposes into multiple structural stories depending on the vector and biological pathway. This story captures the system-level institutional failure (coordination without externality pricing). Downstream constraints include specific vectors (fungal pathogens in shipping pallets, cryptic invertebrates in soil ballast), regional invasion cascades (island biotas, freshwater systems), and quarantine agency capacity limits (biosecurity underfunding). Each has its own ε value reflecting different observables. The network links these as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_trade_externalities_neobiota, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
