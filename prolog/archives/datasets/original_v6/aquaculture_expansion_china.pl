% ============================================================================
% CONSTRAINT STORY: aquaculture_expansion_china
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aquaculture_expansion_china, []).

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
 *   constraint_id: aquaculture_expansion_china
 *   human_readable: Aquaculture Expansion in China
 *   domain: environmental/economic/regulatory
 *
 * SUMMARY:
 *   China's aquaculture sector has expanded from 27% of global production in
 *   2000 to over 60% by 2025, driven by government mandates for food security
 *   and export revenue. This expansion creates a structural constraint that
 *   simultaneously coordinates legitimate functions (rural employment,
 *   protein production, export earnings) while extracting from coastal
 *   ecosystems and displacing traditional fishers. The constraint exhibits a
 *   full perspectival spectrum: for aquaculture producers, it appears as pure
 *   coordination (Rope); for coastal ecosystems and displaced fishers, it
 *   appears as pure extraction (Snare); for regional governments and central
 *   authorities, it appears as mixed coordination-extraction (Tangled Rope);
 *   for environmental regulators, it appears as a performative compliance
 *   theater (Piton). The extractiveness value (0.58) reflects
 *   moderate-to-high asymmetric extraction partially obscured by genuine
 *   coordination benefits. The suppression value (0.65) reflects structural
 *   barriers to both exit and voice: artisanal fishers cannot retrain or
 *   relocate; ecosystems cannot organize; environmental officials face
 *   political costs for enforcement. The theater ratio (0.58) reflects that
 *   environmental compliance machinery exists but is substantially
 *   performative — facilities violate limits, inspections are sporadic,
 *   penalties are negotiable. Over the 10-year measurement interval, both
 *   extractiveness and theater have increased, indicating growing reliance on
 *   enforcement theater to mask ecological deterioration.
 *
 * KEY AGENTS:
 *   - Aquaculture Producers: Primary beneficiary (institutional/arbitrage) — capture government support, land access exemptions, export market preference; can relocate operations if one region enforces standards
 *   - Artisanal Fishers: Primary victim (powerless/trapped) — displaced by wild stock depletion and coastal pollution; geographically bound, lacking alternative skills; no collective organizing mechanism
 *   - Coastal Ecosystems: Primary victim (powerless/trapped) — abstract collective good; cannot organize; bears pollution, habitat degradation, escape impacts; degradation is partially irreversible
 *   - Regional Governments: Institutional beneficiary and constraint bearer (institutional/constrained) — revenue dependent; subject to central mandates; can impose local standards but face central pressure and producer resistance
 *   - Central Government/Ministry of Agriculture: Institutional driver (institutional/constrained) — mandates expansion for food security and export growth; benefits from revenues; constrained by international reputation costs and domestic environmental pressure
 *   - Environmental Compliance System: Institutional theater (institutional/constrained) — regulations exist but enforcement is sporadic; permits routine violations; provides political cover for expansion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices (maximizing output) as inherent biological limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aquaculture_expansion_china, 0.58).
domain_priors:suppression_score(aquaculture_expansion_china, 0.65).
domain_priors:theater_ratio(aquaculture_expansion_china, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aquaculture_expansion_china, extractiveness, 0.58).
narrative_ontology:constraint_metric(aquaculture_expansion_china, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(aquaculture_expansion_china, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aquaculture_expansion_china, tangled_rope).
narrative_ontology:human_readable(aquaculture_expansion_china, "Aquaculture Expansion in China").
narrative_ontology:topic_domain(aquaculture_expansion_china, "environmental/economic/regulatory").

domain_priors:requires_active_enforcement(aquaculture_expansion_china).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aquaculture_expansion_china, aquaculture_producers).
narrative_ontology:constraint_beneficiary(aquaculture_expansion_china, export_market_operators).
narrative_ontology:constraint_beneficiary(aquaculture_expansion_china, government_fisheries_revenue).
narrative_ontology:constraint_victim(aquaculture_expansion_china, wild_fish_stocks).
narrative_ontology:constraint_victim(aquaculture_expansion_china, coastal_ecosystems).
narrative_ontology:constraint_victim(aquaculture_expansion_china, small_scale_fishers).
narrative_ontology:constraint_victim(aquaculture_expansion_china, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ARTISANAL FISHER (SNARE) — Structurally trapped by geographic dependency and lack of alternative livelihood skills. Aquaculture expansion depletes wild stocks and contaminates fishing grounds, eliminating income source. No exit options: cannot retrain easily, cannot relocate without severing community ties. Bears full cost of expansion with zero coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(aquaculture_expansion_china, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COASTAL ECOSYSTEM COMMONS (SNARE) — Cannot organize, cannot exit. Bears pollution (nutrient runoff, antibiotics, escaped farmed fish) and habitat degradation. Abstract collective good with no representation in expansion decisions. Suppression is structural: ecosystem degradation is irreversible on short timescales; recovery requires halting expansion, which is politically costly.
constraint_indexing:constraint_classification(aquaculture_expansion_china, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL GOVERNMENT (TANGLED ROPE) — Constrained by revenue dependency and central mandates for food security and export earnings. Aquaculture provides real coordination function: cheap protein production, rural employment, export revenue. But also enforces extraction from environment and fishers. Faces pressure from Beijing for production targets and from local fishers for livelihood protection. Has some agency (can impose environmental standards) but limited exit (depends on aquaculture revenues; cannot pivot quickly to alternative economies).
constraint_indexing:constraint_classification(aquaculture_expansion_china, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AQUACULTURE PRODUCER INDUSTRY (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism: government support for land access, exemptions from environmental enforcement, subsidized feed grain, export market preference. Net flow of benefits runs toward this actor. Can arbitrage between regions (relocate operations if one area enforces standards). Low experienced extraction — this actor benefits from the constraint's enforcement.
constraint_indexing:constraint_classification(aquaculture_expansion_china, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL GOVERNMENT (TANGLED ROPE) — Drives expansion through production mandates and export growth targets (coordination function: food security, rural income). Simultaneously constrained by international reputation costs (pollution incidents cause export bans) and domestic environmental pressure. Benefits from expansion revenues but also bears cost of ecosystem collapse and international criticism. Has enforcement authority but faces political costs of stopping growth. Active enforcement required to maintain the constraint.
constraint_indexing:constraint_classification(aquaculture_expansion_china, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL COMPLIANCE SYSTEM (PITON) — Environmental impact assessments, coastal zone management regulations, and pollution limits are largely performative. Facilities routinely exceed permitted feed input, antibiotics, and discharge limits; inspections are infrequent and penalties are negotiable. Theater ratio is high (0.58) because regulatory machinery persists despite low enforcement effectiveness. Constraint is maintained through inertia (regulations exist on paper) rather than functional verification. Theatrical compliance provides political cover for expansion while permitting actual environmental degradation.
constraint_indexing:constraint_classification(aquaculture_expansion_china, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a biological/thermodynamic perspective, aquaculture's resource intensity (feed conversion ratios, waste concentration) creates structural friction that appears immutable: producing 1kg of farmed fish requires 2-3kg of feed grain, generating concentrated waste. This can seem like a natural law of aquaculture. But the structural data reveals this as contingent institutional choice: alternative practices (lower stocking density, polyculture, feed optimization) reduce but don't eliminate the constraint. The 'immutability' is actually policy choice (maximizing output over sustainability) naturalized as biology.
constraint_indexing:constraint_classification(aquaculture_expansion_china, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aquaculture_expansion_china_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aquaculture_expansion_china, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aquaculture_expansion_china, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aquaculture_expansion_china, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(aquaculture_expansion_china, TR),
    TR >= 0.70.

:- end_tests(aquaculture_expansion_china_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetric distribution of benefits and costs. Aquaculture producers and export markets capture substantial economic benefits during the expansion phase. Coastal ecosystems and wild fish stocks bear costs that are diffuse, delayed, and partially irreversible. Artisanal fishers face immediate income loss but lack political voice to demand compensation. The extractiveness value reflects that this is not pure coordination (which would show ε ≤ 0.35) nor pure extraction (which would show ε ≥ 0.70). The coordination component is genuine — aquaculture does produce protein and rural employment — but it is achieved through asymmetric distribution of environmental and livelihood costs. Suppression (0.65): Reflects multiple interlocking barriers to exit and voice. Artisanal fishers cannot exit fishing (no alternative livelihoods, geographic dependency, cultural identity); cannot voice (atomized, unorganized, politically marginalized). Ecosystems cannot exit or voice. Regional governments cannot exit (revenue dependency, central mandates) but have some voice (can implement local standards). Producers face some suppression (pollution incidents cause export bans; international sustainability standards) but have high exit capacity (can relocate). Theater ratio (0.58): Environmental compliance machinery is partially performative. Impact assessments and pollution monitoring exist but are often rubber-stamped. Penalties for violations are typically negotiable and do not exceed profit margins. Facility inspections are infrequent and often pre-announced. The theater has increased over the measurement interval as environmental opposition has grown, requiring more elaborate legitimation mechanisms (certifications, reports, official statements) while actual enforcement has not strengthened proportionally.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The beneficiary (aquaculture industry) sees Rope: pure coordination of legitimate protein production and export value. The victim populations (artisanal fishers, coastal ecosystems) see Snare: pure extraction with no coordination benefit, only displacement and degradation. The regional government sees Tangled Rope: genuine coordination benefits (employment, revenue) alongside real extraction costs (environmental management, fisher displacement pressure) with limited agency. The central government sees Tangled Rope at a different level: driving expansion (coordination) while constrained by reputation costs and internal pressure (extraction). The environmental compliance system sees Piton: maintains performative machinery (inspections, reports, standards) despite low functional effectiveness. The analytical observer risks seeing Mountain: viewing resource intensity limits as immutable biological laws rather than as outcomes of policy choice (maximizing output over sustainability). This perspectival spectrum reveals that the constraint's 'type' is not intrinsic but depends entirely on the observer's structural position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationship chain. Aquaculture producers are beneficiaries with arbitrage exit options (can relocate to less regulated regions or countries); derivation yields low d, negative chi — this agent benefits from the constraint. Artisanal fishers are victims with trapped exit options (geographic and skill binding); derivation yields high d, high chi — this agent bears maximal extraction. Regional governments are simultaneously beneficiary (revenue) and victim (enforcement costs, ecosystem damage liability); structural derivation yields moderate-high d reflecting their constrained position. The central government drives expansion (low extraction-bearing d) but faces international and domestic pressure (higher d than pure beneficiary status). Environmental systems have no exit options, only extraction — highest d in the system. The pipeline computes chi from d, applying the sigmoid f(d) and scope modifier σ(S). Regional scope (σ=0.9) dampens chi slightly compared to global scope (σ=1.2), reflecting that national-level environmental accountability is weaker than global trade standard accountability.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint simultaneously coordinates legitimate functions AND extracts from powerless agents. This is the defining structure of Tangled Rope: genuine coordination (food security, rural employment, export earnings) paired with asymmetric extraction (environmental degradation, livelihood displacement, suppressed alternative pathways). The misclassification risk is treating aquaculture expansion as pure coordination (Rope) because the coordination benefits are real and politically salient. The mandatrophy flag reveals that this framing conceals the extraction structure: the beneficiaries are concentrated (producers, exporters, regional officials), while the costs are diffuse (ecosystem commons, unorganized fishers, future generations). The Snare classifications (for powerless agents) and Piton classification (for theater) are not contradictions of the primary Tangled Rope diagnosis but rather perspectival readings that reveal what the aggregate Tangled Rope obscures. The central government's Tangled Rope classification at generational timescale reflects that expansion appears sustainable when evaluated only at immediate/biographical scales (coordination benefits outweigh visible costs) but becomes unsustainable at generational scale (ecosystem collapse, wild stock depletion, social friction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feed_source_substitutability,
    'Can aquaculture''s feed grain dependency be fundamentally reduced through alternative feed sources (insects, algae, plant-based) or is high extraction inherent?',
    'Comparative lifecycle analysis of alternative feed systems; market adoption rates and scaling feasibility; cost differential vs conventional grain',
    'If alternatives scale: constraint moves from Snare (trapped ecosystem) to Scaffold (temporary problem with technological sunset). If locked into grain: extraction mechanism is structural and permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feed_source_substitutability, empirical, 'Whether alternative feed sources can reduce aquaculture resource intensity').

omega_variable(
    ecosystem_recovery_reversibility,
    'Are pollution impacts and coastal ecosystem degradation reversible within generational timescales if aquaculture halts, or is the constraint creating permanent environmental trajectory change?',
    'Longitudinal ecosystem monitoring in areas where aquaculture has ceased; sediment core analysis for contaminant persistence; fish stock recovery timelines post-closure',
    'If reversible (10-20 years): suppression is high but not absolute; constraint could shift to Scaffold with sunset. If irreversible: suppression approaches 1.0; Snare classification solidifies for coastal ecosystems and wild stocks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_recovery_reversibility, empirical, 'Whether coastal ecosystem damage from aquaculture is reversible').

omega_variable(
    artisanal_fisher_coalition_formation,
    'Can displaced artisanal fishers overcome collective action barriers to organize politically, or are they individually locked into compliance?',
    'Historical analysis of fisher organizing movements in China and globally; measurement of coalition coordination costs vs individual adaptation costs; policy responsiveness to organized fisher pressure',
    'If coalition forms: powerless/trapped agents potentially upgrade to organized/constrained, changing classification perspectives and creating pressure on regional governments. If they remain atomized: constraint persists as Snare for this population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artisanal_fisher_coalition_formation, empirical, 'Whether artisanal fishers can form political coalitions against aquaculture expansion').

omega_variable(
    central_mandate_enforcement_credibility,
    'Do central government environmental mandates reflect genuine commitment to ecosystem limits or are they performative signals that regional governments are expected to work around?',
    'Analysis of enforcement patterns: inspection frequency, penalty magnitude, production growth despite stated limits; cross-province comparison of enforcement stringency; official guidance documents to regional authorities',
    'If mandates are genuine: environmental suppression is real structural constraint, limiting expansion and potentially shifting piton toward rope (functional compliance). If performative: theater ratio interpretation is correct; mandates serve as political cover without constraining production.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(central_mandate_enforcement_credibility, empirical, 'Whether central environmental mandates are enforced or performative').

omega_variable(
    international_market_standard_internalization,
    'Do export market sustainability standards (EU, Japan import rules) create genuine external pressure that modifies producer behavior, or do Chinese producers arbitrage between markets (meeting standards for high-price markets, violating for lower-standard buyers)?',
    'Documentation of certification patterns; pricing differentials by market; audit failure rates and remediation; traceability system compliance verification',
    'If standards bite: external constraints shift power balance toward environmental compliance; Tangled Rope classifications become more rope-like (more coordination, less extraction). If arbitraged: standards are pure theater; beneficiary power remains concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_market_standard_internalization, empirical, 'Whether international sustainability standards constrain or are arbitraged by Chinese aquaculture producers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aquaculture_expansion_china, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aqua_tr_t0, aquaculture_expansion_china, theater_ratio, 0, 0.42).
narrative_ontology:measurement(aqua_tr_t5, aquaculture_expansion_china, theater_ratio, 5, 0.5).
narrative_ontology:measurement(aqua_tr_t10, aquaculture_expansion_china, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(aqua_be_t0, aquaculture_expansion_china, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aqua_be_t5, aquaculture_expansion_china, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(aqua_be_t10, aquaculture_expansion_china, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aquaculture_expansion_china, resource_allocation).
narrative_ontology:affects_constraint(aquaculture_expansion_china, wild_fish_stock_collapse_southeast_asia).
narrative_ontology:affects_constraint(aquaculture_expansion_china, coastal_pollution_china).
narrative_ontology:affects_constraint(aquaculture_expansion_china, artisanal_fisher_displacement).

% DUAL FORMULATION NOTE:
% Aquaculture expansion in China is a single integrated constraint with multiple victim populations and beneficiary streams. Related constraints (wild stock collapse, coastal pollution, fisher displacement) are causal downstream effects that should be decomposed into separate stories per the ε-invariance principle. Wild fish stock collapse has its own extractiveness value reflecting the biological dynamics and trade in wild-caught fish; coastal pollution has its own value reflecting ecosystem degradation rates; artisanal displacement has its own story reflecting labor market dynamics. All three are causally downstream of and structurally linked to aquaculture expansion via network.affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aquaculture_expansion_china, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
