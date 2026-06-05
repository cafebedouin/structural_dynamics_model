% ============================================================================
% CONSTRAINT STORY: drug_trafficking_supply_chain_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_drug_trafficking_supply_chain_fragmentation, []).

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
 *   constraint_id: drug_trafficking_supply_chain_fragmentation
 *   human_readable: Drug Trafficking Supply Chain Fragmentation
 *   domain: organized_crime/law_enforcement/geopolitics
 *
 * SUMMARY:
 *   Drug trafficking supply chain fragmentation is a structural adaptation of
 *   criminal distribution networks to enforcement pressure and profit
 *   maximization constraints. The constraint operates across nine distinct
 *   structural positions — from dependent users (trapped, maximum extraction)
 *   through mid-level traffickers (constrained exit, high extraction) to
 *   cartel hierarchies (powerful, arbitrage options, mixed
 *   coordination-extraction) to enforcement bureaucracies (maintaining
 *   institutional structures despite low functional effectiveness). The
 *   fragmentation itself — modular supply chains with multiple producers,
 *   routes, and distributors — serves a dual function: reducing enforcement
 *   risk for trafficking organizations while simultaneously creating
 *   consumer-facing quality uncertainty, pricing extraction, and health risks
 *   for users. The constraint manifests as a Snare for powerless actors
 *   (users, source-region communities), a Tangled Rope for powerful and
 *   institutional actors (cartels, source governments), a Piton for law
 *   enforcement institutions (performative enforcement with declining real
 *   effectiveness), and a Rope for destination-market coordinators
 *   (benefiting from supply modularity). The analytical observer risks
 *   naturalizing this as an immutable feature of black markets (Mountain)
 *   when the fragmentation structure is actually a contingent product of
 *   specific policy and enforcement regimes.
 *
 * KEY AGENTS:
 *   - End Consumers/Dependent Users: Primary victim (powerless/trapped) — face physiological and psychological dependency, legal prohibition, criminal extraction. Maximum suppression, zero exit options.
 *   - Source-Region Communities: Primary victim (powerless/trapped) — geographic and economic entrapment in drug-producing areas. Multigenerational suppression, limited alternatives.
 *   - Low-Level Traffickers and Dealers: Secondary victim (moderate/constrained) — face cartel enforcement and law enforcement incarceration. Theoretical exit but extreme costs approach trapped status.
 *   - Cartel Hierarchies and Major Traffickers: Primary beneficiary (powerful/arbitrage) — benefit from fragmented supply chains while enforcing compartmentalization. Genuine coordination function alongside extraction.
 *   - Source-Country Governments: Mixed (institutional/constrained) — coordinate security and development while extracting through taxation and population control. Tangled Rope experience.
 *   - Law Enforcement and Drug Control Agencies: Institutional actor (institutional/constrained) — maintain enforcement structures with declining real effectiveness. Piton: performative continuation through inertia.
 *   - Transit-Country Governments: Institutional coordinator (institutional/constrained) — manage border security and regional stability. Rope: coordination dominates extraction.
 *   - Destination-Market Distributors: Beneficiary (institutional/arbitrage) — benefit from supply modularity and redundancy. Rope: coordination benefits.
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent policy structure as natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drug_trafficking_supply_chain_fragmentation, 0.68).
domain_priors:suppression_score(drug_trafficking_supply_chain_fragmentation, 0.75).
domain_priors:theater_ratio(drug_trafficking_supply_chain_fragmentation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drug_trafficking_supply_chain_fragmentation, extractiveness, 0.68).
narrative_ontology:constraint_metric(drug_trafficking_supply_chain_fragmentation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(drug_trafficking_supply_chain_fragmentation, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(drug_trafficking_supply_chain_fragmentation, snare).
narrative_ontology:human_readable(drug_trafficking_supply_chain_fragmentation, "Drug Trafficking Supply Chain Fragmentation").
narrative_ontology:topic_domain(drug_trafficking_supply_chain_fragmentation, "organized_crime/law_enforcement/geopolitics").

domain_priors:requires_active_enforcement(drug_trafficking_supply_chain_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(drug_trafficking_supply_chain_fragmentation, end_consumers).
narrative_ontology:constraint_victim(drug_trafficking_supply_chain_fragmentation, source_region_communities).
narrative_ontology:constraint_victim(drug_trafficking_supply_chain_fragmentation, transit_country_populations).
narrative_ontology:constraint_victim(drug_trafficking_supply_chain_fragmentation, law_enforcement_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT USER (SNARE) — End consumers face high suppression and extraction with zero exit options. Addiction creates physiological and psychological dependency; legal prohibition eliminates legitimate supply channels; criminal supply chains extract premium pricing and quality uncertainty. Maximum experienced extraction: trapped within both the constraint structure and the material condition of addiction. No arbitrage, no mobility, no alternative.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOURCE REGION COMMUNITIES (SNARE) — Coca-growing regions, poppy-farming areas, methamphetamine precursor source communities face extreme suppression: land-use dependence, limited alternative crops, cartel enforcement, and eradication programs. Geographic entrapment compounds economic entrapment. Extraction flows from communities to trafficking organizations and beyond. Multiple generations born into the system perceive it as unchangeable despite high extractiveness.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LOW-LEVEL TRAFFICKERS AND DEALERS (SNARE) — Street-level distributors, mid-level dealers, and low-hierarchy traffickers face high suppression from both law enforcement and cartel hierarchy. Exit is theoretically possible (constrained) but carries extreme costs: cartel enforcement prevents defection; law enforcement incarceration is severe; alternative livelihoods in source regions are scarce. Experience approximates trapped. High experienced extraction despite theoretical exit.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CARTEL HIERARCHY / MAJOR TRAFFICKERS (TANGLED ROPE) — High-level trafficking organizations experience the fragmented supply chain as a coordination problem that generates revenue. Fragmentation (multiple producers, routes, distributors) reduces interdiction risk and enables price discrimination. They benefit from the constraint structure while also investing in enforcing it (maintaining compartmentalization, punishing defection). Genuine coordination function: supply chain resilience through modularity. Also genuine extraction: internal taxation, monopolistic pricing, enforcement hierarchy. Both present simultaneously — not pure extraction, not pure coordination.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DRUG ENFORCEMENT BUREAUCRACY (PITON) — Law enforcement and drug control agencies experience the supply chain as a persistent target that justifies ongoing resource allocation and institutional structure. The constraint's institutional maintenance is largely performative: decades of enforcement show declining seizure-to-flow ratios and rising purity at street level, yet drug control budgets and agency structures persist. Theater ratio reflects ritualized enforcement (press releases about seizures, interdiction statistics) that masks structural failure. Institutional inertia maintains the system despite low functional effectiveness at reducing supply or consumption.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOURCE COUNTRY GOVERNMENTS (TANGLED ROPE) — Governments in production regions coordinate security, regulatory, and development functions while also extracting through taxation and control. They benefit from drug-trade taxation (estimated 3-5% of GDP in some regions), intelligence gathering, and leverage over rural populations. Genuine coordination: territorial control, security provision, infrastructure. Genuine extraction: taxation, forced participation in eradication programs, asymmetric resource distribution. Both present — some countries manage hybrid extraction-coordination; others skew toward extraction.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TRANSIT COUNTRY GOVERNMENTS (ROPE) — Governments of countries used for trafficking routes experience the constraint primarily as a coordination problem: managing border security, controlling violence, negotiating with international partners. Many extract limited benefit from trafficking (corruption rents are modest compared to source or destination countries). High suppression for transit populations but moderate coordination function for governments managing geopolitical relationships and institutional security. Sees constraint as largely coordination with significant enforcement costs.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: DESTINATION MARKET DISTRIBUTORS (ROPE) — Consumer-market distributors (pharmaceutical wholesalers for diverted medications, street-level dealers in high-income countries) experience fragmented supply as a coordination mechanism: multiple independent suppliers reduce dependency on any single source; redundancy increases reliability; compartmentalization limits exposure if one node is interdicted. Benefits from modularity while contributing to retail price. Lower suppression than production-region actors — legal and market alternatives exist. Rope classification: coordination benefits dominate.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational view, the fragmented structure is an inevitable property of any large-scale illicit supply system under enforcement pressure: prohibition creates demand that supply chains must adapt to avoid detection; optimal structure under enforcement is distributed and compartmentalized; this is inherent to any black-market organization. However, the structural data reveals this as false naturalization: fragmentation is a contingent choice, not a law of physics. Decriminalization or harm-reduction frameworks could produce different equilibria.
constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drug_trafficking_supply_chain_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(drug_trafficking_supply_chain_fragmentation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(drug_trafficking_supply_chain_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(drug_trafficking_supply_chain_fragmentation, TR),
    TR >= 0.70.

:- end_tests(drug_trafficking_supply_chain_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Base extraction starts at 0.52 (early 2000s: more centralized production and distribution, higher interdiction rates, lower consumer prices but volatile supply) and rises to 0.68 (current: fragmented networks, lower interdiction rates, higher street prices, stable supply, quality standardization enables monopolistic pricing). The rise reflects cartel learning and adaptation — the fragmented structure enables greater extraction per unit distributed through redundancy (premium pricing for supply certainty) and compartmentalization (reduced competition among producers by market segmentation). Suppression (0.75): Consistently high across all powerless and trapped agents. Legal prohibition creates primary suppression; cartel enforcement creates secondary suppression; physiological addiction creates tertiary suppression. For source-region communities, geographic entrapment and limited alternatives add structural suppression. Theater ratio (0.42): Moderate, declining. Law enforcement's public-facing activity (interdiction announcements, seizure statistics) is partly performative — seizure-to-total-flow ratios have declined from ~20% (1990s) to ~5-10% (current), yet enforcement budgets remain stable. Theater is lower than pure law-enforcement-theater constraints because actual violence and incarceration do occur (not purely performative); but a significant portion of enforcement activity (high-profile seizures, international coordination announcements) produces minimal functional impact on supply or consumption. Theater rises slightly over the interval as enforcement response becomes more ritualized despite declining real effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Dependent users experience a pure Snare (no coordination, maximum extraction, zero agency). Cartels experience a Tangled Rope (genuine coordination of supply modularity paired with asymmetric extraction through hierarchy and violence). Law enforcement experiences a Piton (performative institutional ritual). Source governments experience Tangled Rope (coordination of state capacity with extraction of taxation and population control). The same fragmented supply structure appears as pure extraction to powerless agents and as genuine coordination plus fair-return hierarchy to powerful agents. The perspectival gap reveals that 'fragmentation' is not an objective feature — it is a coordination mechanism that distributes costs and benefits extremely asymmetrically. The analytical observer's Mountain perspective (fragmentation is inevitable in any illicit supply system) is a false summit: decriminalization frameworks, harm reduction models, and pharmaceutical-supply alternatives (Switzerland, Portugal, medical cannabis) demonstrate that different policy regimes produce fundamentally different supply structures with different extraction profiles. What appears natural from the enforcement perspective is revealed as contingent from cross-policy analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural position — their power level, exit options, and beneficiary/victim status. Dependent users: d ≈ 0.98 (powerless + trapped + victim = maximum target). Source communities: d ≈ 0.95 (powerless + trapped + victim across generations = near-maximum). Low-level traffickers: d ≈ 0.75 (moderate power + constrained exit that approximates trapped + victim to cartel hierarchy = high target, but with some organizational role). Cartels: d ≈ 0.05 (powerful + arbitrage + beneficiary = near-minimum, actually subsidized by the constraint). Law enforcement: d ≈ 0.50 (institutional power + constrained by policy + mixed beneficiary-victim status — benefits from institutional resources, victimized by resource constraints and declining effectiveness). Transit governments: d ≈ 0.45 (institutional + constrained + mixed — modestly benefits from security provision, costs paid by enforcement and population). The sigmoid f(d) translates these d values into experienced extractiveness multipliers: dependent users experience approximately 1.42× the base extractiveness; cartels experience approximately -0.12× (subsidization). Scope scaling (σ(S) = 1.2 for global scope) amplifies chi for powerless agents: a powerless user with trapped exit and global scope experiences χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (experienced extraction exceeds base extraction due to being the primary target in a globally-scaled system).
 *
 * MANDATROPHY ANALYSIS:
 *   The Snare classification prevents misidentification of fragmentation as pure coordination. A Rope classification would suggest that supply modularity serves genuine coordination across all actors — but users face zero coordination benefits and maximum extraction. The Snare gate (ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66) is satisfied from the user and source-community perspectives, preventing false coordination diagnosis. The Tangled Rope classification for cartels and source governments correctly identifies that these actors do experience genuine coordination benefits alongside extraction — the fragmented structure provides redundancy and information sharing value. The Piton classification for law enforcement prevents misidentifying performative enforcement as actual coordination — the theater_ratio rise from 0.35 to 0.42 reveals degradation of real functional capacity while institutional structures persist. The mountain classification risk comes from the universal/civilizational analytical view ('fragmentation is inevitable in black markets') — but the comparative evidence from different policy regimes shows that the fragmented structure is contingent on prohibition policy, not a natural law of supply organization. The mandatrophy is resolved by recognizing that the six types are legitimate perspectival readings: the same structural phenomenon is Snare, Tangled Rope, Piton, Rope, and contingently Mountain depending on agent position and policy regime. No single type is 'the truth' — the presheaf of perspectives across all observation points constitutes the full classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_driver_ambiguity,
    'Is supply chain fragmentation primarily driven by law enforcement pressure, cartel organizational strategy for profit maximization, or both equally?',
    'Comparative analysis of trafficking networks in different enforcement regimes (high-enforcement vs harm-reduction jurisdictions); historical analysis of supply chain structure changes following major enforcement operations or policy shifts',
    'If enforcement-driven: reducing enforcement reduces fragmentation and may increase consumer safety through standardization. If profit-driven: enforcement changes create only marginal adaptation; addressing extraction requires demand-side intervention. If mixed: different leverage points for different subpopulations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_driver_ambiguity, empirical, 'Whether fragmentation is enforcement-driven or profit-driven').

omega_variable(
    suppression_mechanism_intergenerational,
    'Is the high suppression (0.75) maintained through structural barriers alone or through internalized learned helplessness in source-region communities?',
    'Post-policy intervention analysis: if suppression persists after structural barriers are removed (land access, crop alternatives, cartel enforcement reduced), the binding mechanism includes internalization. Pre-post design comparing communities with and without alternative livelihood programs.',
    'If structural: reducing barriers reduces suppression. If internalized: psychological/social recovery support is required alongside structural change. If both: addressing suppression requires dual intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intergenerational, empirical, 'Structural vs internalized suppression in source communities').

omega_variable(
    harm_reduction_extraction_floor,
    'What is the irreducible minimum extraction in drug supply (product quality uncertainty, health risks, monopolistic pricing) even under harm-reduction or regulated frameworks?',
    'Historical comparison of pharmaceutical supply chains (regulated) vs illicit supply chains (unregulated); analysis of consumer-reported harms and pricing in jurisdictions with decriminalization or medical access (Switzerland, Portugal, Oregon).',
    'If floor ≈ 0.15: most extraction is contingent policy choice, not structural necessity. If floor ≈ 0.40: significant extraction is inherent to any supply system; policy can only reduce, not eliminate. Affects whether Snare is misclassification or accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_extraction_floor, empirical, 'Irreducible extraction minimum in any drug supply system').

omega_variable(
    cartel_coordination_vs_coercion_ratio,
    'In the Tangled Rope perspective (cartel hierarchy), what proportion of internal coordination is voluntary (revenue-sharing, information exchange) vs coerced (hierarchy enforcement, violence)?',
    'Analysis of cartel-member testimony, defector interviews, organizational structure documentation; comparison of organizational persistence in high-violence regimes vs lower-violence coordination mechanisms.',
    'If coercion > 70%: Tangled Rope misclassifies; should be Snare for low-level members, Rope+Snare hybrid for mid-level. If coercion < 50%: Tangled Rope correctly captures genuine coordination with asymmetric benefit distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cartel_coordination_vs_coercion_ratio, empirical, 'Ratio of voluntary coordination to coercion in cartel hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(drug_trafficking_supply_chain_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dtscf_tr_t0, drug_trafficking_supply_chain_fragmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dtscf_tr_t10, drug_trafficking_supply_chain_fragmentation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(dtscf_tr_t20, drug_trafficking_supply_chain_fragmentation, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(dtscf_be_t0, drug_trafficking_supply_chain_fragmentation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(dtscf_be_t10, drug_trafficking_supply_chain_fragmentation, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(dtscf_be_t20, drug_trafficking_supply_chain_fragmentation, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(drug_trafficking_supply_chain_fragmentation, resource_allocation).
narrative_ontology:boltzmann_floor_override(drug_trafficking_supply_chain_fragmentation, 0.18).
narrative_ontology:affects_constraint(drug_trafficking_supply_chain_fragmentation, pharmaceutical_supply_chain_diversion).
narrative_ontology:affects_constraint(drug_trafficking_supply_chain_fragmentation, precursor_chemical_trafficking).
narrative_ontology:affects_constraint(drug_trafficking_supply_chain_fragmentation, money_laundering_infrastructure).

% DUAL FORMULATION NOTE:
% Supply chain fragmentation decomposes into three structurally distinct constraints: (1) pharmaceutical supply diversion (ε ≈ 0.35, Tangled Rope — some coordination of generic-to-brand substitution, but extraction of profit margin), (2) precursor trafficking (ε ≈ 0.72, Snare — pure extraction to source chemical suppliers and transit countries), (3) money laundering (ε ≈ 0.55, Tangled Rope — coordination of financial opacity with extraction of transaction costs). The fragmentation constraint aggregates these three; decomposition enables separate analysis of each supply layer's specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(drug_trafficking_supply_chain_fragmentation, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
