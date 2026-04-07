% ============================================================================
% CONSTRAINT STORY: incumbent_rent_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_rent_extraction, []).

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
 *   constraint_id: incumbent_rent_extraction
 *   human_readable: Incumbent Rent Extraction Through Regulatory Moats
 *   domain: economic_political
 *
 * SUMMARY:
 *   Incumbent rent extraction through regulatory moats represents a
 *   structural tension between legitimate coordination (uniform standards,
 *   quality assurance, market stability) and asymmetric extraction (barriers
 *   to entry that protect profits beyond competitive levels). The constraint
 *   exhibits the classic tangled rope signature: genuine coordination
 *   function coexists with asymmetric extraction that requires active
 *   regulatory enforcement. The beneficiary (incumbent firm) and the
 *   regulatory agency maintain the barrier structure through formal rules,
 *   licensing requirements, approval timelines, and standard-setting
 *   processes that newcomers must navigate at prohibitive cost. The
 *   extractiveness has increased over the measurement interval (0.42 → 0.63)
 *   as incumbents have layered additional regulatory complexity, while
 *   theater ratio has increased (0.38 → 0.55) as the formal justifications
 *   become increasingly performative — the original coordination function
 *   persists but the extraction function has grown disproportionately. The
 *   constraint demonstrates the power of analyzing from multiple structural
 *   positions: incumbents see pure coordination (rope), entrants see pure
 *   extraction (snare), regulatory agencies see mixed coordination and
 *   capture (tangled rope with identity-lock dynamics), and organized
 *   challengers see a temporary institutional arrangement with a sunset
 *   (scaffold).
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — extract rents through regulatory protection; experience constraint as coordination mechanism
 *   - Potential Entrants: Primary victim (powerless/trapped) — face insurmountable barriers; no negotiating power; cannot exit without leaving market entirely
 *   - Regional Challengers: Secondary victim (moderate/constrained) — can enter through niche strategies but face material entry costs and predatory incumbent responses
 *   - Regulatory Agencies: Institutional actor (institutional/constrained with identity-lock risks) — coordinate standards while captured by incumbent influence; career and budget dependencies create lock-in
 *   - Consumers: Dispersed victim (powerless/trapped) — experience price inflation and reduced choice; cannot organize collective action against incumbent rents
 *   - Anti-Monopoly Coalition: Organized agent (organized/mobile) — consumer advocates, new-economy firms, disruptive technologies; see sunset via antitrust reform and disruption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_rent_extraction, 0.58).
domain_priors:suppression_score(incumbent_rent_extraction, 0.65).
domain_priors:theater_ratio(incumbent_rent_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_rent_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_rent_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(incumbent_rent_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_rent_extraction, tangled_rope).
narrative_ontology:human_readable(incumbent_rent_extraction, "Incumbent Rent Extraction Through Regulatory Moats").
narrative_ontology:topic_domain(incumbent_rent_extraction, "economic_political").

domain_priors:requires_active_enforcement(incumbent_rent_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_rent_extraction, incumbent_firms).
narrative_ontology:constraint_beneficiary(incumbent_rent_extraction, regulatory_agencies).
narrative_ontology:constraint_victim(incumbent_rent_extraction, entrants_and_potential_competitors).
narrative_ontology:constraint_victim(incumbent_rent_extraction, consumers_via_price_inflation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% New competitors face overwhelming barriers: licensing requirements, capital thresholds, regulatory approval timelines, and incumbent-friendly standard-setting. Exit means abandoning the market entirely; cost of entry grows faster than expected returns. Maximum extraction with zero negotiating power.
constraint_indexing:constraint_classification(incumbent_rent_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Can enter through geographic niche or specialization, but faces significant costs: regulatory compliance, incumbent predatory pricing, customer lock-in. Benefits from some coordination function (standardized markets reduce their own transaction costs) while bearing extraction costs. Mixed experience: some agency, substantial friction.
constraint_indexing:constraint_classification(incumbent_rent_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Primary beneficiary. Experiences the constraint as pure coordination: regulatory standards reduce their internal compliance costs, raise competitor barriers, and protect market position. Benefits flow consistently; exit would mean surrendering market control. Positive directionality, low effective extraction.
constraint_indexing:constraint_classification(incumbent_rent_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Institutional perspective with constrained exit. Genuinely coordinates market standards (genuine function), but also captured by incumbent industry influence. Derives budget, staff expertise, and career pathways from incumbent relationships. Mixed: coordination and extraction both present. Cannot exit without losing institutional capacity.
constraint_indexing:constraint_classification(incumbent_rent_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Organized pressure from consumer advocates, new-economy firms, and regional competitors sees incumbent rents as a temporary institutional artifact. Antitrust enforcement, technology disruption, and regulatory reform create exit pathways. Low effective extraction because the coalition has agency and sees a sunset to incumbent dominance via digital disruption, platform alternatives, or enforcement action.
constraint_indexing:constraint_classification(incumbent_rent_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Institutional inertia: regulatory frameworks designed for incumbent protection persist through legislative resistance to reform, even as market conditions shift. Rent extraction mechanism weakens as technology and global competition bypass geographic moats, but the formal structure (licensing, approval timelines, standard-setting boards) persists. Theater ratio high because the formal machinery continues performing protection functions that economic reality has partially overcome.
constraint_indexing:constraint_classification(incumbent_rent_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From analytical/civilizational perspective, rent extraction by incumbents appears as an immutable feature of competitive markets: incumbents always have advantages (scale, brand, capital access), and some extraction from entrants is inherent to market structure. This perspective naturalizes what is actually a contingent institutional arrangement (regulatory barriers). The engine's false summit detector will flag this as naturalization rather than genuine natural law.
constraint_indexing:constraint_classification(incumbent_rent_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_rent_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_rent_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_rent_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_rent_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_rent_extraction, TR),
    TR >= 0.70.

:- end_tests(incumbent_rent_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Incumbent rents above competitive returns represent direct extraction from consumers and blocked entrants. The value (0.58 rather than 0.75+) reflects that some barriers serve genuine coordination functions (quality standards, consumer safety, market transparency) that would be partially replicated even in competitive regimes. Suppression (0.65): High. Entry barriers include: licensing requirements with incumbent-friendly criteria, capital thresholds beyond startup capacity, regulatory approval timelines (5-10 years in regulated sectors), incumbent control of standard-setting, customer switching costs from incumbent scale advantages, predatory pricing during incumbent challenge. Agents cannot exit without forgoing market access entirely. Theater ratio (0.55): Moderate-high. The formal regulatory machinery has legitimate coordination functions (preventing race-to-the-bottom in quality, managing externalities, protecting consumer information asymmetries), but increasingly serves theatrical legitimation for rent extraction. As technology has reduced the coordination necessity (e.g., software can replace manual inspection, reputation systems replace monopoly quality guarantees), the machinery persists largely to protect incumbent positions. The measurement trajectory shows theater increasing faster than extractiveness, indicating regulatory justifications are becoming proportionally more performative even as the extraction itself grows.
 *
 * PERSPECTIVAL GAP:
 *   The constraint displays maximal perspectival divergence. The incumbent sees rope (pure coordination of market standards), while potential entrants see snare (pure extraction with no coordination benefit to them). Regulatory agencies see tangled rope (genuine coordination function + capture), which conflicts with both the incumbent's rope perception (coordination is natural and mutually beneficial) and the entrant's snare perception (coordination is fake). The anti-monopoly coalition sees scaffold (temporary arrangement disrupted by technology and reform), which frames both the current rent extraction and the regulatory coordination as institutional artifacts rather than natural or necessary. The legacy protection coalition (institutional inertia perspective) sees the same formal machinery as persisting through political entrenchment, not through functional necessity. This perspectival ecosystem perfectly exemplifies the mandate of indexical classification: the constraint is not 'really' any single type, but rather a presheaf where each structural position generates a legitimate but partial view. The false summit risk occurs when analysts adopt the analytical/civilizational view and naturalize incumbent advantage as inevitable economic law, missing the contingent institutional scaffolding that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent position. Incumbent firms (beneficiaries with arbitrage exit) experience low d (~0.15), producing negative χ — the constraint subsidizes them. Potential entrants (victims with trapped exit) experience high d (~0.95), producing χ > 1.0 — maximum extracted burden. Regional challengers (victims with constrained exit) experience moderate-high d (~0.70). Regulatory agencies occupy a complex position: ostensibly institutional beneficiaries (agencies benefit from incumbent relationships, career pathways, budget flows), but also victims of institutional capture that constrains their autonomy. This dual position creates an institutional identity_locked dynamic — the agency has constrained exit (could reform, but would lose institutional capacity and industry relationships it depends on) combined with victim status (locked into defending arrangements that may not serve public interest). The anti-monopoly coalition has mobile exit (can shift focus to other political priorities), producing moderate d (~0.50-0.55). The analytical observer risks d (~0.72) if they naturalize incumbent advantage as inherent market law, rather than recognizing contingent regulatory choice.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by demonstrating that tangled rope is the analytically correct type, not by arguing that coordination and extraction are indistinguishable. The genuine coordination function (market standards, quality assurance, consumer protection) justifies regulatory frameworks. The genuine extraction function (entry barriers that protect profits beyond competitive levels) creates asymmetric burden. Both are structurally real. The mandatrophy is dissolved by recognizing that tangled rope is specifically designed to handle this case — it models constraints that coordinate AND extract. The scaffold perspective provides the critical check: if technology or reform can sustainably replace the coordination function while removing the extraction function, the current arrangement is indeed contingent (not natural law). The piton perspective provides the inertia check: some of the formal machinery persists through institutional path-dependence even as its functional necessity declines. The snare perspective from potential entrants is NOT the 'true' classification — it represents the victim's inability to see the coordination benefits (real, but not in their interest). The rope perspective from incumbents is NOT the 'true' classification — it represents the beneficiary's natural blindness to their own extraction. The tangled rope is the analytical classification: both functions present, both structurally real, both mathematically measurable through the χ formula decomposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_versus_regulatory_moat,
    'How much of incumbent advantage derives from natural economies of scale versus regulatory barriers versus network effects versus switching costs?',
    'Comparative analysis across jurisdictions with different regulatory intensity; measurement of entry barriers before and after deregulation; cross-sector analysis of comparable markets with different regulation levels.',
    'If primarily regulatory: constraint is snare/tangled_rope with high malleability via policy change. If primarily natural: constraint approaches mountain (irreducible). If mixed: constraint remains tangled_rope but sunset timeline differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_versus_regulatory_moat, empirical, 'Natural versus regulatory contribution to incumbent moat').

omega_variable(
    regulatory_agency_capture_depth,
    'Is the regulatory agency captured by incumbents (identity-locked institutional actor) or merely constrained by institutional design and political economy?',
    'Analysis of agency personnel flows (revolving door), funding source dependencies, technical standard-setting processes, enforcement patterns against incumbent versus entrant violations.',
    'If captured: agency should be analyzed as identity_locked, changing its classification. If constrained: agency sees genuine coordination function alongside extraction constraint. Changes directionality calculations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_agency_capture_depth, empirical, 'Regulatory agency structural relationship: capture versus constraint').

omega_variable(
    technology_disruption_timeline,
    'What is the realistic timeline for technology (digital platforms, decentralized systems, geographic arbitrage) to bypass incumbent regulatory moats?',
    'Historical precedent (telecom deregulation, internet platform entry, energy transition), current disruption trajectories in the specific sector, capital concentration in disruptive alternatives.',
    'If 5-10 years: scaffold sunset is structural and measurable. If 20+ years: scaffold classification is aspirational rather than structural. If <2 years: scaffold may underestimate the speed of regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_disruption_timeline, empirical, 'Realistic timeline for technology-driven incumbent moat erosion').

omega_variable(
    consumer_surplus_extraction_magnitude,
    'What percentage of consumer surplus is extracted via incumbent rents relative to total value? Are consumers better off with regulated incumbent monopoly than with unregulated chaos?',
    'Price comparison across regulated and underegulated jurisdictions for similar services; consumer welfare analysis including quality, reliability, and externality costs; counterfactual analysis of deregulated market outcomes.',
    'If extraction > 30% and regulation provides no reliability benefit: snare classification confirmed. If extraction < 15% and regulation provides substantial public goods: tangled_rope confirmed with higher coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_surplus_extraction_magnitude, empirical, 'Magnitude of consumer surplus extraction and regulation trade-off').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_rent_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incrent_tr_t0, incumbent_rent_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(incrent_tr_t10, incumbent_rent_extraction, theater_ratio, 10, 0.42).
narrative_ontology:measurement(incrent_tr_t20, incumbent_rent_extraction, theater_ratio, 20, 0.52).
narrative_ontology:measurement(incrent_tr_t30, incumbent_rent_extraction, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(incrent_be_t0, incumbent_rent_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(incrent_be_t10, incumbent_rent_extraction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(incrent_be_t20, incumbent_rent_extraction, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(incrent_be_t30, incumbent_rent_extraction, base_extractiveness, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_rent_extraction, resource_allocation).
narrative_ontology:affects_constraint(incumbent_rent_extraction, regulatory_capture).
narrative_ontology:affects_constraint(incumbent_rent_extraction, professional_licensing_gatekeeping).
narrative_ontology:affects_constraint(incumbent_rent_extraction, platform_network_effects_moat).

% DUAL FORMULATION NOTE:
% Incumbent rent extraction manifests differently across sectors: utility monopolies (telecommunications, electricity), professional services (law, medicine via licensing), platform dominance (digital markets), and natural resource extraction (mining, logging permits). Each sector-specific instantiation has its own extractiveness value depending on technology disruption likelihood and regulatory entrenchment depth. This story provides the general structural model; sector-specific stories decompose via the affects_constraints network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incumbent_rent_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
