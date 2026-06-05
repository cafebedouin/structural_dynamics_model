% ============================================================================
% CONSTRAINT STORY: smallholder_tenure_insecurity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smallholder_tenure_insecurity, []).

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
 *   constraint_id: smallholder_tenure_insecurity
 *   human_readable: Smallholder Tenure Insecurity and Extractive Land Governance
 *   domain: economic/agricultural/political
 *
 * SUMMARY:
 *   Smallholder tenure insecurity describes the structural vulnerability of
 *   farming communities whose land rights are not formally recognized or
 *   legally protected by the state. This constraint operates across
 *   Sub-Saharan Africa, South Asia, Latin America, and Southeast Asia,
 *   affecting approximately 2 billion people. The constraint exhibits
 *   tangled_rope properties: it contains genuine coordination functions
 *   (consolidating fragmented holdings for large-scale investment, reducing
 *   administrative burden, enabling market participation through
 *   collateralizable credit) alongside asymmetric extraction (smallholders
 *   bear dispossession risk while benefits flow to state agencies, commercial
 *   investors, and development funders). The institutional framework is
 *   inherited from colonial land administration and has been maintained
 *   post-independence despite its degradation. Extractiveness has increased
 *   over the 20-year interval (0.38 → 0.58) as commercial agricultural
 *   investment and land commodification have intensified. Theater ratio has
 *   risen (0.35 → 0.52) as the legitimacy framing has shifted from
 *   efficiency/development to climate adaptation and carbon sequestration,
 *   creating new extractive claims on smallholder land. Suppression has
 *   intensified (0.52 → 0.68) as states and investors have deployed more
 *   aggressive enforcement mechanisms (security forces, legislative
 *   restriction of customary rights, acceleration of
 *   titling-for-dispossession programs). The constraint is analytically rich
 *   because it permits all six classification types from different structural
 *   positions, making it an exemplar for how indexical classification reveals
 *   the same phenomenon as fundamentally different structural experiences
 *   depending on the agent's power and exit options.
 *
 * KEY AGENTS:
 *   - Smallholder Farming Communities: Primary victims (powerless/trapped) — bear full extraction cost; no alternative livelihood; tenure loss = subsistence loss + identity loss + children's displacement
 *   - Indigenous Land Stewards and Pastoral Communities: Specific victims with even lower state recognition (powerless/trapped) — pre-colonial land claims actively suppressed by formal titling
 *   - Women Farmers: Disproportionate victims (powerless/trapped + identity_locked) — tenure typically vests in household heads (male), excluding women despite their farming role; customary land loss eliminates autonomous income and inheritance security
 *   - State Land Agencies: Primary beneficiaries (institutional/arbitrage) — administrative simplification, revenue (titling fees, land sales), development metrics (hectares allocated)
 *   - Commercial Agricultural Investors: Primary beneficiaries (institutional/arbitrage) — access to consolidated land, reduced negotiation complexity, export market linkages, capital accumulation
 *   - International Development Funders: Secondary beneficiaries (powerful/mobile) — narrative success (property rights), development metrics, portfolio returns through impact investment vehicles
 *   - Land Rights Advocacy Coalition: Organized challengers (organized/constrained) — building alternative tenure frameworks (community land trusts, customary codification, participatory mapping) with generational sunset timeline
 *   - Colonial Land Registration Framework (institutional inertia): Piton-class mechanism persisting through theater and beneficiary lock-in despite degraded coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smallholder_tenure_insecurity, 0.58).
domain_priors:suppression_score(smallholder_tenure_insecurity, 0.68).
domain_priors:theater_ratio(smallholder_tenure_insecurity, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, extractiveness, 0.58).
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(smallholder_tenure_insecurity, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smallholder_tenure_insecurity, tangled_rope).
narrative_ontology:human_readable(smallholder_tenure_insecurity, "Smallholder Tenure Insecurity and Extractive Land Governance").
narrative_ontology:topic_domain(smallholder_tenure_insecurity, "economic/agricultural/political").

domain_priors:requires_active_enforcement(smallholder_tenure_insecurity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, state_land_agencies).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, commercial_agricultural_investors).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, agribusiness_corporations).
narrative_ontology:constraint_beneficiary(smallholder_tenure_insecurity, international_development_funders).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, smallholder_farming_communities).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, indigenous_land_stewards).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, pastoral_communities).
narrative_ontology:constraint_victim(smallholder_tenure_insecurity, women_farmers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by material and legal barriers. Land dependency is absolute; no alternative livelihood infrastructure exists. Formal recognition requires engagement with state bureaucracy they cannot navigate or afford. Suppression is structural and complete: losing access to ancestral land means losing subsistence, losing children's inheritance, losing identity as a farming household. The constraint extracts surplus labor, restricts crop choices, enables predatory crediting, and forecloses generational accumulation. Maximum experienced extraction with zero exit capacity.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALLHOLDER COOPERATIVE (TANGLED ROPE) — Limited collective power can negotiate with state and investors, accessing some markets and credit. But coordination is partial: competing individual interests, elite capture within cooperatives, and state co-optation fragment collective action. The constraint both enables (pooling land for large-buyer contracts) and extracts from (cooperative revenues captured by managers, unequal distribution of buyer contracts). Exit cost is high (losing collective access, losing market linkage) but not impossible. Moderate experience of extraction with some agency.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE LAND AGENCY (ROPE) — Experiences tenure insecurity as a coordination mechanism: formalizing titles requires consolidation, which requires reducing the number of land claims, which requires aggregating small plots into sellable units. The agency's core function is enabled by the insecurity: without it, their administrative load increases (managing millions of micro-plots), their revenue decreases (transaction fees on small transfers), and their development mandate (measured in hectares allocated to commercial agriculture) becomes harder to achieve. The constraint solves their coordination problem. They experience it as necessary bureaucratic infrastructure, not extraction. Net beneficiary with high exit optionality (can change tenure rules if political incentives shifted).
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL AGRICULTURAL INVESTOR (ROPE) — Tenure insecurity solves their coordination problem: without it, they must negotiate with millions of smallholders, compensate each, manage complex rights-bundling, and contend with communal claims. Tenure insecurity reduces this complexity to a state negotiation (one party) and a land clearing operation (relocate/marginalize existing users). The constraint enables large-scale monoculture, export agriculture, and capital-intensive mechanization. They experience the constraint as solving the 'tragedy of the commons' — natural law justification for consolidation. Net beneficiary; high arbitrage optionality (can invest elsewhere if tenure regime changes).
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT FUNDER (TANGLED ROPE) — Land formalization is a canonical development intervention: property rights enable credit access, investment incentives, and market participation. The funder genuinely coordinates market entry and financial system integration for smallholders. BUT the funder's metrics reward land under formal title (hectares registered), not smallholder income or food security. This misalignment extracts value: formalization often triggers land concentration (smallholders sell under economic pressure, investors consolidate holdings), debt traps (credit becomes available but unpayable from marginal farm output), and market integration that exports profit (cash crops sold to multinational traders at commodity prices). The funder benefits from the narrative of market-making; smallholders often experience dispossession. Both coordination and extraction present.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: LAND RIGHTS ADVOCACY COALITION (SCAFFOLD) — Organized civil society actors (NGOs, farmer unions, indigenous councils) mobilizing against tenure insecurity are building alternative governance pathways: community land trusts, participatory land mapping, customary rights codification, and legal recognition of informal tenure. These alternatives have sunset logic: as community-based tenure systems mature and gain legal standing, the state monopoly on land authority erodes. The coalition experiences the constraint as temporary and contestable. Their exit path is real (alternative institutions being built) and their agency is significant. But the timeline is long (generational) and state/investor resistance is high (suppression for coalition members is significant).
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: COLONIAL LAND REGISTRATION FRAMEWORK (PITON) — The institutional form of tenure insecurity (state-monopolized formal titling) is inherited from colonial administration (demarcating colonial territory, centralizing land authority, extracting tax revenue, enabling resource extraction). Post-independence states have maintained this framework despite its institutional degradation: formal titling is slow, expensive, inaccessible to the poor, and often produces contested rather than secure title (competing claims from different state agencies, legal ambiguities about pre-colonial rights). The theater ratio is high: the formal system performs legitimacy and state modernity while actual security remains low for those it claims to protect. The framework persists through institutional inertia and the beneficiary class (state agencies, investors) that depends on it, not because it functions as designed. Piton classification: degraded coordination mechanism maintained by theater and beneficiary lock-in.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, land scarcity and population growth create inevitable pressure toward consolidation and intensification. Smallholding fragmentation is an inefficient allocation mechanism; formal titling and large-scale agriculture are the inevitable evolution. Tenure insecurity is merely the friction cost of this transition — a natural law of agricultural development. However, this perspective naturalizes what is actually a contingent political choice: the extraction is not inevitable but enforced; the consolidation beneficiaries are not all of society but a specific class; the alternatives (community tenure, cooperative consolidation, smallholder intensification) are structurally viable but politically opposed. False summit candidate.
constraint_indexing:constraint_classification(smallholder_tenure_insecurity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smallholder_tenure_insecurity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smallholder_tenure_insecurity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smallholder_tenure_insecurity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smallholder_tenure_insecurity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(smallholder_tenure_insecurity, TR),
    TR >= 0.70.

:- end_tests(smallholder_tenure_insecurity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from smallholders through multiple mechanisms: (1) dispossession under legal pressure (losing land = losing capital), (2) labor extraction (insecurity incentivizes low-cost farming inputs and high effort), (3) suppressed crop choice (insecurity agents select for quick-return commodity crops, reducing nutritional diversity and increasing market dependency), (4) credit trapping (insecurity makes smallholders prefer formal credit at high rates over community lending), (5) suppressed investment (no tenure security reduces incentive to improve soil or water infrastructure). But extractiveness is not maximal (not 0.72+) because: (a) genuine coordination benefits exist (some smallholders gain access to buyer contracts and credit they otherwise couldn't obtain), (b) not all smallholders are displaced (many maintain marginal holdings despite insecurity), (c) some regions show higher smallholder resilience and lower extraction. The 0.58 value reflects the tangled-rope structure: real coordination mixed with real asymmetric extraction. Suppression (0.68): High but not total. Structural barriers include: (1) formal titling cost (unaffordable for poor smallholders), (2) literacy/documentation barriers (smallholders lack birth certificates, ownership documents), (3) state access barriers (agencies geographically distant, bureaucracy incomprehensible), (4) legal barriers (customary land claims not recognized, married women excluded), (5) security forces enforcing investor claims against smallholder resistance. But suppression is not absolute: (a) some smallholders navigate formalization despite barriers, (b) customary tenure retains some de facto security, (c) advocacy coalitions are reducing barriers (community land mapping, NGO legal support). The 0.68 reflects significant but not total suppression. Theater ratio (0.52): Moderate-high. Substantial performative content includes: (1) property rights rhetoric (formalization presented as rights protection when often preceding dispossession), (2) development narrative (land consolidation justified as agricultural modernization and poverty reduction despite often worsening food security), (3) climate framing (land acquisition justified as carbon sequestration or climate-smart agriculture despite carbon-heavy export crops), (4) state capacity theater (land agencies claim to be protecting rights while selectively enforcing against smallholders). But theater is not dominant (not 0.70+) because: (a) genuine institutional processes exist (some titling does get completed), (b) actual land transfers do occur (not purely performative), (c) real coordination benefits do flow in some cases. The 0.52 reflects significant performative framing layered on real but asymmetric institutional processes. The rising trajectory (0.35 → 0.52) reflects increasing rhetorical framing of land consolidation as climate mitigation, ESG compliance, and development impact—narratives that have reduced transparency about dispossession costs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence: the same institutional arrangement is experienced as Snare (victim), Rope (state agency), Rope (investor), Tangled Rope (cooperative and funder), Scaffold (advocacy coalition), Piton (colonial framework), and false-summit Mountain (analytical naturalization). The gaps reveal the structural reality: what appears as a coordination problem to the state and investors appears as pure extraction to smallholders. What appears as temporary friction (scaffold perspective) to advocates appears as permanent dispossession to victims. What appears as natural law evolution (mountain) is actually a maintained institutional choice that benefits specific classes. The perspectival gap is not epistemological confusion—it reflects genuine structural divergence in how the constraint functions for different agents. The smallholder farmer's Snare classification (trapped, maximum extraction) is not a misunderstanding of the state's Rope classification; it is the structural reality for someone with zero exit capacity and land dependency. The state agency's Rope classification (coordination) is also structurally accurate for their functional position—they do solve an administrative coordination problem through tenure insecurity. Both classifications are correct for their respective contexts. The analytical insight is that the constraint performs different structural functions for different agents: coordination for the powerful, extraction for the powerless. Recognition of this gap enables diagnosis: the constraint cannot be reformed through coordination improvements alone (more efficient titling) because the extraction is functionally necessary to the beneficiary class. Reform requires either (a) power rebalancing (smallholder organization or state capacity to enforce against investor pressure), or (b) institutional replacement (community tenure systems gaining legal standing, making state-monopoly tenure unnecessary).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect each agent's structural position in the extraction flow. Smallholder farmers are full victims (d ≈ 0.95): structurally trapped, no arbitrage options, tenure loss eliminates their primary livelihood. The sigmoid f(d) ≈ 1.42 amplifies their experienced extractiveness (χ = 0.58 × 1.42 × 1.0 ≈ 0.82 at national scope). State land agencies are net beneficiaries (d ≈ 0.05): extracting value (tenure fees, land sales, development metrics) with complete arbitrage optionality (can change tenure rules). The sigmoid f(d) ≈ -0.12 dampens their experienced extractiveness, making it appear as coordination (low effective extraction). Commercial investors are beneficiaries (d ≈ 0.10): consolidating land at reduced transaction cost, with high arbitrage optionality. The sigmoid f(d) ≈ -0.01 makes the extraction nearly invisible to them—they experience pure coordination benefit. Development funders occupy (d ≈ 0.48, powerful/mobile): moderately positioned between beneficiary framing and victim outcomes, with high exit optionality (can redeploy capital elsewhere). The sigmoid f(d) ≈ 0.60 produces moderate χ. The cooperative perspective (moderate/constrained) has intermediate directionality (d ≈ 0.55): victims of investor competition but beneficiaries of buyer contracts, constrained exit (losing collective access is costly). The advocacy coalition (organized/constrained) has high directionality (d ≈ 0.65): opposing the extraction but facing suppression. The state agency and investor perspectives both derive beneficiary+arbitrage → low d → low/negative f(d), making the constraint appear as coordination from their structural position. Smallholder perspectives derive victim+trapped → high d → high f(d), making the constraint appear as maximal extraction. The directionality divergence is not a measurement error—it is the structural reality that the constraint extracts from some and benefits others, producing genuine perspectival classification divergence. No override is necessary; the base derivation captures the actual structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by mapping all six types to the fundamental question: 'Is tenure insecurity a coordination mechanism (market-making, administrative simplification) or an extraction mechanism (dispossession, rent capture)?' The answer is: both, for different agents. The state and investor perspectives see genuine coordination (Rope). The victim perspectives see genuine extraction (Snare). The tangled-rope classifications (cooperatives, funders, advocates) see both. The piton classification (colonial framework) sees degraded coordination maintained by theater. The mountain classification risks naturalizing the choice as inevitable. The mandatrophy is resolved not by selecting one type but by recognizing that the constraint performs different structural functions depending on power position. This recognition enables precise diagnosis: the constraint is NOT a coordination problem to be solved by more efficient titling (this would improve the Rope function while deepening the extraction). It is a redistribution mechanism that benefits smallholders only if rebalancing occurs (advocacy coalition/scaffold perspective) or if alternatives gain institutional standing (community tenure). The constraint cannot be 'fixed' within the state-formalization framework; it can only be replaced by institutional alternatives. The mandatrophy-resolution insight is that tenure insecurity is a stable equilibrium for beneficiary classes precisely because it performs both genuine coordination (for them) and asymmetric extraction (for victims) simultaneously. Removing the extraction would require removing the coordination benefits they derive, which they will resist. Reform requires institutional innovation (community tenure gaining legal parity) or power rebalancing (smallholder organization reaching critical mass), not incremental improvements to the existing framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_formality_paradox,
    'Does formal title certification actually increase smallholder security, or does it trigger dispossession by making land tradable and creating debt-trap collateral?',
    'Longitudinal household panel data comparing smallholders with formal vs informal tenure over 10-20 years, tracking land retention, debt levels, income, and food security outcomes across regions with different investor pressure',
    'If formalization increases security: tenure insecurity is a coordination problem (Rope from multiple perspectives). If formalization increases dispossession: it is an extraction mechanism disguised as property rights (Snare from victim perspective). Current evidence is mixed and region-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_formality_paradox, empirical, 'Whether formal title increases or decreases smallholder security').

omega_variable(
    community_tenure_effectiveness,
    'Can customary and community-based tenure systems (without state formalization) provide sufficient security and credit access for smallholder advancement, or is state-backed titling a necessary condition for market integration?',
    'Comparative case studies of community-managed land systems (Ethiopian debo/dega, Tanzanian village land councils, indigenous territory management, pastoral commons) measuring tenure security, credit access, investment levels, and food security relative to formal-title regions',
    'If community tenure is effective: the state-monopoly extraction mechanism is not structurally necessary; alternatives exist (Scaffold perspective confirmed). If community tenure fails at scale: state formalization is structurally necessary (Mountain perspective gains support).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_tenure_effectiveness, empirical, 'Whether community-based tenure systems can provide comparable security without state formalization').

omega_variable(
    extraction_vs_coordination_boundary,
    'At what point does the legitimate coordination function of consolidating fragmented holdings become predatory extraction of smallholder land?',
    'Define threshold metrics: consolidation pace (hectares per year), smallholder displacement rate, investor land acquisition cost vs smallholder forgone productivity, compensation adequacy relative to household relocation cost. Empirical mapping of regional tenure trajectories against these metrics.',
    'If extraction begins early (low displacement rates already problematic): constraint is primarily Snare. If extraction requires high displacement rates: constraint permits Rope/Tangled Rope interpretations in low-pressure regions. Boundary is empirical, not conceptual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Threshold metrics distinguishing coordination from predatory consolidation').

omega_variable(
    state_capacity_constraint,
    'Is tenure insecurity caused by structural state incapacity (too many claims to adjudicate, insufficient resources for land administration), or by deliberate enforcement choices (maintaining insecurity to enable land concentration)?',
    'Comparison of state investment in land administration infrastructure and personnel across regions with different tenure security outcomes. Analysis of de jure vs de facto enforcement: do states with formal property law frameworks actually enforce them, or does enforcement selectively support investor interests?',
    'If incapacity: tenure insecurity is partly a coordination problem requiring institutional investment (Rope/Scaffold perspective). If deliberate: insecurity is a maintained extraction mechanism (Snare perspective, false-summit mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_constraint, empirical, 'Whether tenure insecurity reflects state incapacity or deliberate enforcement choices').

omega_variable(
    alternative_consolidation_models,
    'Could consolidation happen through voluntary smallholder cooperation (pooled marketing, cooperative land management) rather than through formal titling and dispossession?',
    'Empirical case studies of cooperative consolidation models (cotton buying groups in West Africa, dairy pools in East Africa, fruit marketing cooperatives in South Asia) measuring consolidation benefits, smallholder retention, profit distribution, and resilience to commodity price shocks relative to investor-driven consolidation',
    'If cooperative consolidation delivers comparable benefits with smallholder retention: the extraction is not necessary (Snare → Tangled Rope reclassification). If cooperatives fail or underperform: investor-driven consolidation gains legitimacy as necessity (Mountain perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_consolidation_models, empirical, 'Whether cooperative consolidation can deliver scale benefits without dispossession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smallholder_tenure_insecurity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_tr_t0, smallholder_tenure_insecurity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tenure_tr_t10, smallholder_tenure_insecurity, theater_ratio, 10, 0.45).
narrative_ontology:measurement(tenure_tr_t20, smallholder_tenure_insecurity, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(tenure_be_t0, smallholder_tenure_insecurity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tenure_be_t10, smallholder_tenure_insecurity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(tenure_be_t20, smallholder_tenure_insecurity, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tenure_su_t0, smallholder_tenure_insecurity, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(tenure_su_t10, smallholder_tenure_insecurity, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(tenure_su_t20, smallholder_tenure_insecurity, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smallholder_tenure_insecurity, resource_allocation).
narrative_ontology:boltzmann_floor_override(smallholder_tenure_insecurity, 0.18).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, smallholder_credit_access).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, agricultural_export_dependency).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, commons_enclosure_dynamics).
narrative_ontology:affects_constraint(smallholder_tenure_insecurity, indigenous_land_dispossession).

% DUAL FORMULATION NOTE:
% Smallholder tenure insecurity is a constraint family node. Upstream constraints (colonial land administration, state centralization, commercial agriculture expansion) create preconditions. Downstream constraints (credit access barriers, export crop dependency, indigenous dispossession) inherit the tenure insecurity structure. Each member has its own ε value: state formalization (ε≈0.55), customary suppression (ε≈0.65), development-funder conditionality (ε≈0.42). The family is linked by institutional dependency: all downstream constraints depend on the upstream tenure insecurity for their extraction mechanisms to function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(smallholder_tenure_insecurity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
