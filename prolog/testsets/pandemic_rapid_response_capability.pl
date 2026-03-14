% ============================================================================
% CONSTRAINT STORY: pandemic_rapid_response_capability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pandemic_rapid_response_capability, []).

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
 *   constraint_id: pandemic_rapid_response_capability
 *   human_readable: Pandemic Rapid Response Capability: Coordination vs. Institutional Extraction
 *   domain: public_health/biosecurity/institutional_governance
 *
 * SUMMARY:
 *   Pandemic rapid response capability creates a structural tension between
 *   genuine coordination needs (developing novel vaccines, scaling
 *   manufacturing, distributing across global supply chains) and extractive
 *   mechanisms (IP monopolies, unequal access, supply hoarding). The
 *   constraint exhibits core properties of Tangled Rope: a real coordination
 *   function (mobilizing research and production) exists alongside asymmetric
 *   extraction (vaccine dose concentration in wealthy nations, price markup
 *   on life-saving medicines, subordination of equity to speed). The
 *   structural data shows extractiveness rising over the pandemic interval
 *   (0.25→0.58) as initial emergency coordination gave way to systematic
 *   supply chain control. Theater ratio similarly increased (0.35→0.62),
 *   indicating growing performative equity commitments alongside actual
 *   inequitable distribution. The constraint operates at multiple scales:
 *   individual hospitals compete for vaccines (local), nations negotiate
 *   procurement (national), and global supply chains concentrate production
 *   geographically (global). Low-income populations with the highest disease
 *   burden and fewest exit options experience this as pure extraction
 *   (Snare); wealthy nations' authorities experience it as coordination
 *   serving national emergency (Rope); organized global health coalitions see
 *   a temporary problem with sunset pathways (Scaffold); the IP regime
 *   persists through institutional inertia (Piton). The analytical observer
 *   risks naturalizing the access inequality as inherent to pandemic biology
 *   rather than recognizing it as a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Low-income populations in high-risk zones: Primary victims (powerless/trapped) — bear highest disease burden; zero exit options; preventable mortality
 *   - Middle-income nations & healthcare systems: Secondary victims (moderate/constrained) — delayed and unequal access; constrained by capital and regulatory barriers; benefit from coordination but unequally
 *   - Pharmaceutical manufacturers: Primary beneficiaries (institutional/arbitrage) — capture production contracts, IP value, market expansion; exit options include licensing, compulsory licensing waivers, government indemnity
 *   - Emergency response authorities (wealthy nations): Primary beneficiaries (institutional/arbitrage) — gain political legitimacy and public health control through rapid rollout; security framing enables hoarding
 *   - Global health coalitions: Organized advocates (organized/constrained) — WHO, Gavi, CEPI, equity advocates pushing technology transfer and access commitments; constrained by reliance on pharma cooperation; building sunset mechanisms
 *   - International IP and trade institutions: Institutional actors (institutional/arbitrage) — maintain IP monopolies, export controls; see own structures as degraded during emergency; benefiting from carve-outs and waivers that prove structural necessity debatable
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choices as biological necessities; must distinguish genuine coordination constraints from extractive institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pandemic_rapid_response_capability, 0.58).
domain_priors:suppression_score(pandemic_rapid_response_capability, 0.48).
domain_priors:theater_ratio(pandemic_rapid_response_capability, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pandemic_rapid_response_capability, extractiveness, 0.58).
narrative_ontology:constraint_metric(pandemic_rapid_response_capability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(pandemic_rapid_response_capability, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pandemic_rapid_response_capability, tangled_rope).
narrative_ontology:human_readable(pandemic_rapid_response_capability, "Pandemic Rapid Response Capability: Coordination vs. Institutional Extraction").
narrative_ontology:topic_domain(pandemic_rapid_response_capability, "public_health/biosecurity/institutional_governance").

domain_priors:requires_active_enforcement(pandemic_rapid_response_capability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pandemic_rapid_response_capability, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pandemic_rapid_response_capability, emergency_response_authorities).
narrative_ontology:constraint_beneficiary(pandemic_rapid_response_capability, wealthy_nations).
narrative_ontology:constraint_victim(pandemic_rapid_response_capability, low_income_populations).
narrative_ontology:constraint_victim(pandemic_rapid_response_capability, vaccine_distribution_equity).
narrative_ontology:constraint_victim(pandemic_rapid_response_capability, global_south_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME POPULATIONS (SNARE) — Cannot exit pandemic exposure; trapped by poverty, geography, and infrastructure barriers. Bear full extraction cost through preventable mortality and prolonged lockdowns while wealthy nations secure vaccine supply chains. No alternative pathways; zero degrees of freedom.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME NATIONS (TANGLED ROPE) — Constrained by capital requirements for manufacturing capacity, cold-chain infrastructure, and regulatory approval timelines. Also benefit from emergency response coordination, technical assistance, and eventual vaccine access — though delayed and unequal. Mixed extraction and coordination function; significant agency barriers but not total entrapment.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMA MANUFACTURERS & EMERGENCY AUTHORITIES (ROPE) — Net beneficiaries with exit options. Manufacturers capture production contracts, intellectual property value, and expanded market access. Emergency authorities gain surge capacity and international coordination leverage. Experience the constraint primarily as coordination mechanism for mobilizing resources and expertise. Rapid response systems feed their priorities.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL HEALTH COALITIONS (SCAFFOLD) — Organized agents (WHO, Gavi, CEPI) see rapid response capability as temporary emergency infrastructure with sunset logic. Equity provisions, technology transfer agreements, and distributed manufacturing mandate sunset as sustainable production becomes routine. Constraints recognized as time-bound; exits visible through institutional redesign. Theater moderate because coalition agents actively contest the extraction mechanisms.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADE & IP INSTITUTIONS (PITON) — TRIPS patent protections and export restrictions persist through institutional inertia despite clear equity costs. The IP regime performs its function (incentivizing innovation) but at degraded efficiency during pandemics — waivers, compulsory licensing, and technology transfer agreements all demonstrate the core function can survive without the rigid enforcement. Theater-dominated because the institution maintains formal structures (patent protection) whose functional necessity is contested and whose primary beneficiaries are already secure.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing the biological reality (novel pathogens emerge, immunity takes time to build) as a permanent institutional necessity for unequal access. Some verification lag and capacity constraints are genuine; the extraction and suppression metrics reveal the institutional arrangements around those constraints are contingent, not natural laws.
constraint_indexing:constraint_classification(pandemic_rapid_response_capability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pandemic_rapid_response_capability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pandemic_rapid_response_capability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pandemic_rapid_response_capability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pandemic_rapid_response_capability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pandemic_rapid_response_capability, TR),
    TR >= 0.70.

:- end_tests(pandemic_rapid_response_capability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. Initial phase (0.25) focused on genuine coordination: research prioritization, manufacturing scaling, global distribution systems. As supply accumulated in wealthy nations and manufacturers secured profitable contracts, extractiveness increased. By late pandemic phase (0.58), the constraint primarily served to maintain IP monopolies and supply chain control despite having solved the genuine coordination problems (vaccines were developed and manufactured). The rise reflects institutional capture of coordination infrastructure for extraction purposes. Suppression (0.48): Moderate. Significant barriers to exit include: manufacturing capital requirements, regulatory approval timelines, cold-chain infrastructure, intellectual property restrictions, and geopolitical competition. However, suppression is not total — some nations deployed compulsory licensing, some manufacturers negotiated technology transfer, generic producers emerged. The middle value reflects real barriers partially offset by institutional resistance to total closure. Theater (0.62): Moderate-high. Equity rhetoric (vaccine nationalism framing as 'national emergency,' equity commitments in public statements) increased even as actual distribution remained unequal. Performance ratio rose because the performative commitments (COVAX pledges, equity provisions in agreements) increasingly replaced actual equitable distribution as the mechanism of legitimation. This is classic Goodhart drift: the equity metric (doses pledged) decoupled from equity reality (doses delivered to neediest populations).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals itself in divergent classifications despite identical base properties. The primary beneficiary (institutional/arbitrage) experiences the constraint as Rope: they are solving the genuine coordination problem of mobilizing research and production capacity. For them, rapid response capability enables their expansion. The organized coalition (organized/constrained) experiences it as Scaffold: they see sunset mechanisms (technology transfer, equity provisions) as real and transitional. The institution maintaining IP monopoly (institutional/arbitrage) experiences it as Piton: the patent regime performs a diminished function (IP protections remain but compulsory licensing shows alternatives work), maintained through inertia. The moderate agent (moderate/constrained) experiences it as Tangled Rope: genuine coordination benefits alongside significant extraction through delayed access and price markup. The powerless victim (powerless/trapped) experiences it as Snare: pure extraction with zero coordination benefit — they get vaccines only after wealthy nations have saturated, at prices they cannot afford, when disease prevalence is already declining. The analytical observer (analytical/analytical) risks seeing a Mountain (pandemic biology requires this inequality) but structural analysis reveals a false summit: the inequality is institutional, not biological. This perspectival spread across all six types demonstrates the diagnostic power of the constraint classification system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to extraction flow. Beneficiaries with arbitrage options (pharmaceutical manufacturers, wealthy nation authorities) have low d ≈ 0.15-0.25, experiencing negative effective extraction chi — the constraint subsidizes them. Victims with trap-level barriers (low-income populations) have high d ≈ 0.90, experiencing maximum chi. Middle-income nations with constrained exit (d ≈ 0.55-0.65) experience moderate chi. Organized advocates with mobilized exit paths (d ≈ 0.40) experience lower chi despite victim-adjacent positions, because organization enables leverage. The beneficiary's low d reflects their dominance of the extraction mechanism: they control vaccine IP, manufacturing capacity, and distribution channels. The victim's high d reflects their complete dependence: they have no alternative source of vaccines, cannot manufacture substitutes, and have no negotiating power. The middling agents' d values reflect their partially constrained positions — some capacity (capital, manufacturing capability, diplomatic voice) but not decisive control. These structural d values feed the sigmoid f(d) to compute each perspective's experienced extractiveness chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT AND RESOLVED: The constraint exhibits mandatrophy because it simultaneously contains genuine coordination function (vaccine development, manufacturing, distribution systems are genuinely necessary collective action problems) and asymmetric extraction (IP monopoly, supply hoarding, unequal access). The temptation to mislabel it as pure coordination (Rope) ignores the systematic inequality. The temptation to mislabel it as pure extraction (Snare) ignores the real coordination value that beneficiaries legitimately provide. The Tangled Rope classification resolves this by requiring BOTH: (1) beneficiaries (pharmaceutical manufacturers, wealthy nation authorities), (2) victims (low-income populations), and (3) active enforcement (IP law, export controls, regulatory approval monopolies). All three gates are satisfied: beneficiaries exist and benefit from the constraint; victims exist and bear costs; enforcement mechanisms are active and necessary. The rising theater ratio (0.35→0.62) signals that the coordination function is being replaced by performative equity commitments, which explains why extractiveness increases even as genuine coordination problems (developing vaccines, scaling production) are solved. The measurement trajectory shows extraction accumulation: as the genuine coordination problem solved, the institutional mechanisms persisted and intensified their extraction. This is the classic degradation path: a genuine coordination structure accumulates extractive layers and converts into a Piton (see institutional IP regime perspective) while the core constraint remains Tangled Rope from the beneficiary view and Snare from the victim view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_rent_extraction,
    'How much of the extraction represents genuine coordination costs (R&D, manufacturing, distribution) versus rent extraction through IP monopoly and supply hoarding?',
    'Comparative analysis of manufacturing costs, regulatory approval timelines, and actual vs negotiated pricing; technology transfer agreement outcomes; generics manufacturing capacity deployed during pandemic',
    'If predominantly coordination: classification shifts toward Rope for beneficiary perspectives. If predominantly extraction: classification shifts toward Snare for victim perspectives. Current estimate assumes mixed (Tangled Rope baseline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_rent_extraction, empirical, 'Genuine coordination cost vs rent extraction in pandemic response').

omega_variable(
    supply_chain_choke_point_inevitability,
    'Are the documented supply chain bottlenecks (fill-finish capacity, specialized glass vials, cold chain infrastructure) inherent to the technology or artifacts of concentrated manufacturing geography?',
    'Historical analysis of distributed manufacturing capacity pre-pandemic; counterfactual modeling of outcomes under different geographic distribution strategies; current diversification outcomes',
    'If inherent: suppression (0.48) is justified by genuine coordination complexity. If geographic artifacts: suppression reflects institutional choices to concentrate production, inflating apparent technical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_choke_point_inevitability, empirical, 'Whether supply bottlenecks are inherent or institutional').

omega_variable(
    equity_provisions_sunset_credibility,
    'Do technology transfer agreements and equity provisions have real sunset mechanisms that will transfer capacity and knowledge, or are they performative commitments that will lapse once emergency leverage disappears?',
    'Post-pandemic tracking of technology transfer implementation; capacity retained in recipient nations; patent waiver persistence; pricing trajectory in endemic phase',
    'If credible sunsets: Scaffold classification confirmed. If performative: reclassify as Piton (equity provisions are theater, not sunset). If broken: reclassify as failed extraction mechanism showing residual Snare dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equity_provisions_sunset_credibility, empirical, 'Credibility of equity and technology transfer sunset provisions').

omega_variable(
    identity_lock_wealthy_nation_authorities,
    'Do emergency response authorities in wealthy nations exhibit identity fusion with the rapid response capability itself, where ''national security'' becomes inseparable from vaccine monopoly, making equitable distribution cognitively unthinkable even when feasible?',
    'Discourse analysis of emergency authority statements; gap between stated equity commitments and negotiated outcomes; counterfactual: would authorities have chosen equal distribution if no identity narrative made hoarding ''obvious'' national interest?',
    'If identity-locked: institutional perspectives on rapid response shift from mobile to identity_locked exit, explaining why obvious equity solutions were not implemented despite feasibility. If not identity-locked: extraction is more nakedly intentional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_wealthy_nation_authorities, conceptual, 'Identity fusion of wealthy nation authorities with vaccine monopoly logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pandemic_rapid_response_capability, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prrc_tr_t0, pandemic_rapid_response_capability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prrc_tr_t6, pandemic_rapid_response_capability, theater_ratio, 6, 0.48).
narrative_ontology:measurement(prrc_tr_t12, pandemic_rapid_response_capability, theater_ratio, 12, 0.62).
narrative_ontology:measurement(prrc_tr_t18, pandemic_rapid_response_capability, theater_ratio, 18, 0.58).

% Extraction over time
narrative_ontology:measurement(prrc_be_t0, pandemic_rapid_response_capability, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(prrc_be_t6, pandemic_rapid_response_capability, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(prrc_be_t12, pandemic_rapid_response_capability, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(prrc_be_t18, pandemic_rapid_response_capability, base_extractiveness, 18, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pandemic_rapid_response_capability, resource_allocation).
narrative_ontology:affects_constraint(pandemic_rapid_response_capability, vaccine_intellectual_property_monopoly).
narrative_ontology:affects_constraint(pandemic_rapid_response_capability, supply_chain_concentration_geography).
narrative_ontology:affects_constraint(pandemic_rapid_response_capability, regulatory_approval_asymmetry).

% DUAL FORMULATION NOTE:
% Pandemic rapid response capability decomposes into three structurally distinct constraints with different epsilon values. The response capability itself (ε=0.58, Tangled Rope) represents mixed coordination and extraction. The upstream IP monopoly (ε=0.72+, Snare) enables the extraction lever. The downstream supply chain geography (ε=0.50+, Tangled Rope) sustains unequal access. Each has different beneficiary/victim relationships and different sunset logic. This constraint family is linked through institutional causality: rapid response capability's extraction is only possible because IP monopoly protects manufacturers and geographic supply concentration enables hoarding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pandemic_rapid_response_capability, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
