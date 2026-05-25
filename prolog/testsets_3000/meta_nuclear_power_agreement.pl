% ============================================================================
% CONSTRAINT STORY: meta_nuclear_power_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_nuclear_power_agreement, []).

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
 *   constraint_id: meta_nuclear_power_agreement
 *   human_readable: Meta's direct investment and offtake agreements for advanced nuclear power
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta's direct investment and power purchase agreements for advanced
 *   nuclear reactors (particularly Small Modular Reactors) represent a
 *   structural constraint that operates at the intersection of energy
 *   decarbonization, market consolidation, and regulatory arbitrage. The
 *   constraint arises because a single large consumer (Meta) can negotiate
 *   bilateral agreements with technology developers (SMR vendors) outside the
 *   transparent wholesale electricity market. This creates extraction through
 *   market bypass (reduced price discovery), consolidation of supply, and
 *   externalization of grid planning costs to regional operators.
 *   Simultaneously, the same agreements solve a genuine coordination problem:
 *   matching massive AI demand with low-carbon capacity at scale. The
 *   constraint exhibits both coordination and extraction, making it a
 *   diagnostic exemplar of Tangled Rope classification and the institutional
 *   permission boundary that determines whether it escalates toward pure
 *   market consolidation (Snare) or degrades toward transparent coordination
 *   (Rope).
 *
 * KEY AGENTS:
 *   - Meta Corporation (institutional/arbitrage): Primary beneficiary — captures price stability, guaranteed low-carbon capacity, geographic flexibility, long-term supply security through direct investment and bilateral agreements
 *   - SMR Developers and Vendors (institutional/arbitrage): Primary beneficiary — de-risks capital, guarantees offtake, reduces merchant risk, gains access to Meta's financial resources and data center sites
 *   - Regional Grid Operators and Transmission Companies (moderate/constrained): Victim of market bypass — lose visibility into capacity planning, face reduced competitive supply for other customers, bear grid coordination costs externalized by bilateral agreements
 *   - Competitive Power Generators and Utilities (powerful/constrained): Victims of market consolidation — face reduced wholesale market volume, pricing opacity, and difficulty accessing Meta's demand (a prime customer lost to bilateral market)
 *   - Price Discovery Commons and Wholesale Market Mechanisms (powerless/trapped): Abstract victim — loses volume and representative cost signals as large transactions exit transparent market, undermining pricing accuracy for remaining participants
 *   - Decarbonization Coalition and Climate Advocates (organized/constrained): Mixed beneficiary and victim — benefits from SMR deployment acceleration but constrained by market distortion; sees constraint as temporary bridge technology with structured sunset
 *   - Energy Regulators and FERC (institutional/arbitrage): Institutional actor maintaining theater — oversee wholesale markets and merchant generation but lack enforcement mechanisms to prevent regulatory arbitrage; theater increases as bilateral structures proliferate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nuclear_power_agreement, 0.52).
domain_priors:suppression_score(meta_nuclear_power_agreement, 0.48).
domain_priors:theater_ratio(meta_nuclear_power_agreement, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_nuclear_power_agreement, tangled_rope).
narrative_ontology:human_readable(meta_nuclear_power_agreement, "Meta's direct investment and offtake agreements for advanced nuclear power").
narrative_ontology:topic_domain(meta_nuclear_power_agreement, "technological/economic").

domain_priors:requires_active_enforcement(meta_nuclear_power_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, meta_corporation).
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, smr_developers).
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, power_market_participants).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, regional_grid_operators).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, competitive_power_generators).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, energy_price_discovery).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, grid_reliability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL GRID OPERATORS (SNARE) — Operators and non-aligned generators face extraction through bilateral bypass of the wholesale market. Meta's long-term power purchase agreements lock in capacity and pricing outside transparent auctions, reducing available supply for competitive bidding. Grid planners lose visibility into future demand and supply dynamics. Exit options are trapped: they must operate the grid infrastructure regardless of whether large power users exit the market through private deals.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRICE DISCOVERY COMMONS (SNARE) — The transparent wholesale electricity market depends on aggregating diverse supply and demand. When large consumers (Meta) and developers (SMR vendors) bypass price discovery through bilateral deals, the market loses volume and representative cost signals. This undermines accurate pricing for remaining market participants. The abstract collective good of price transparency has no exit option and bears the cost of extraction through information asymmetry.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GRID RELIABILITY COALITION (TANGLED ROPE) — Grid operators, utility regulators, and resilience engineers benefit from SMR deployment (additional generation capacity, load diversity, heat valorization). However, the bilateral structure between Meta and SMR developers extracts information and control: deployment timing becomes optimization for Meta's data center footprint, not grid reliability; emergency dispatch becomes uncertain (private agreements may reduce availability during system stress). Mixed benefits and extraction.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: META CORPORATION (ROPE) — Experiences the constraint as pure coordination: long-term power purchase agreements solve the fundamental problem of matching massive, predictable AI demand with low-carbon supply. Direct investment in SMR developers aligns incentives and reduces capital barriers. Meta benefits from price stability, geographic flexibility, and guaranteed low-carbon capacity. Exit options are strong: Meta can locate data centers globally and has access to alternative power sources. The constraint is coordination that Meta structures to its advantage.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SMR DEVELOPERS (ROPE) — Offtake agreements de-risk capital development for advanced nuclear: guaranteed customer, long-term revenue, reduced merchant risk. Meta's willingness to sign multi-billion dollar contracts enables financing and deployment at scale. Developers benefit from both coordination (demand aggregation) and extraction (long-term customer lock-in at favorable terms from their perspective). Exit options are arbitrage: vendors can pursue alternative customers or different deployment models.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECARBONIZATION COALITION (SCAFFOLD) — Climate advocates, renewable energy operators, and carbon accounting groups see Meta's SMR agreements as a temporary coordination mechanism accelerating the transition away from fossil fuels. The constraint is a bridge technology: it mobilizes private capital for nuclear deployment while grid decarbonization infrastructure matures. The sunset is structural — as grid carbon intensity falls (renewables + storage), the necessity for bilateral mega-watt agreements declines. The coalition experiences suppression (market distortion) but sees it as transitional, not permanent.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: UTILITY REGULATORS (PITON) — The regulatory framework (FERC Order 2222, state RPS mandates, wholesale market rules) was designed for a world of geographically dispersed generators selling into transparent wholesale markets. Meta's bilateral offtake agreements exploit regulatory arbitrage: the agreements are structured as merchant generation + direct purchase, sidestepping utility-of-record obligations and rate oversight. The regulatory theater (public hearings, cost-benefit analysis, rate case litigation) persists but has diminished functional effect on deployment. Regulators maintain the illusion of oversight while private actors restructure the market underneath.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a system perspective, Meta's agreements solve a genuine coordination problem (matching AI demand with decarbonized power) while simultaneously creating asymmetric extraction (bypassing price discovery, consolidating market power, externalizing grid planning costs). The constraint is not a natural law but a contingent institutional arrangement: the ability to structure bilateral deals outside the wholesale market depends on regulatory permission, which is itself contested. The analytical perspective sees both the coordination function (real) and the extraction mechanism (real).
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nuclear_power_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_nuclear_power_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_nuclear_power_agreement, TR),
    TR >= 0.70.

:- end_tests(meta_nuclear_power_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Meta's offtake agreements extract value through three mechanisms: (1) market bypass (exiting transparent wholesale market reduces pricing pressure), (2) supply consolidation (SMR capacity that could serve competitive market is locked into bilateral contract), (3) grid planning externality (regional operators must plan for unpredictable large consumer via private arrangement, not transparent forecasting). However, extractiveness is not severe (not 0.70+) because SMR capacity is genuinely new (not diverted from existing supply) and Meta's demand response flexibility provides some offsetting grid service. The measurement trajectory shows extractiveness increasing from 0.28 to 0.52 over the interval as bilateral agreements proliferate and lock in longer-term commitments. Suppression (0.48): Moderate. Barriers to competitive access include regulatory arbitrage (bilateral structures exploit gaps in wholesale market rules), capital concentration (only large consumers can negotiate SMR investment), information asymmetry (bilateral negotiations occur outside transparent venues), and path dependency (once Meta has secured supply, remaining generators have weaker negotiating position). However, suppression is not severe (not 0.60+) because regulatory mechanisms exist (FERC can mandate wholesale participation) and competitive generators can organize (industry associations, state-level interventions). Theater ratio (0.35): Low. The bilateral agreements are functionally transparent — the extraction mechanism (market bypass, consolidation, pricing advantage) is visible and explicable. Unlike traditional utility regulation or peer review, there is minimal performative theater: the constraint operates primarily through material market structure, not ritual. The theater ratio declines over the interval as market participants become increasingly explicit about the consolidation dynamic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a fundamental divergence in how market actors perceive the same institutional arrangement. Meta sees coordination (solving the demand-supply matching problem for decarbonized power at scale). SMR developers see coordination with de-risking (achieving capital financing through guaranteed offtake). Grid operators and competitive generators see extraction and market consolidation (losing volume, visibility, and competitive position). The price discovery commons sees degradation of information quality (opacity from bilateral transactions). The decarbonization coalition sees a temporary but necessary bridge (constraint will sunset as renewables + storage mature). Regulators see their own theater (the wholesale market rules persist but their effect diminishes as bilateral deals proliferate). The analytical observer sees the tangled rope clearly: both coordination function and extraction mechanism are real and structural. The perspectival gap arises because different agents have different exit options relative to the constraint. Meta and SMR developers have strong arbitrage exits (both could restructure differently). Grid operators and generators have constrained exits (they must participate in wholesale markets regardless). The price discovery commons has no exit (transparency is a collective good). This structural differentiation in exit options produces the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: beneficiary status, victim status, and exit options. Meta (beneficiary + arbitrage) derives low d (~0.05–0.15), producing negative or near-zero f(d) — effective extraction runs TOWARD Meta, not away from it. Meta experiences χ < 0 (benefit). SMR developers (beneficiary + arbitrage) similarly derive low d, with positive f(d) benefit. Grid operators (victim + constrained) derive higher d (~0.55–0.65), producing elevated f(d) (1.15+) — they experience extraction. Competitive generators (victim + constrained but with some industry organization) derive d ~0.60–0.75, also elevated. The price discovery commons (powerless/trapped) derives maximum d (~0.95), producing maximum f(d) (~1.42) — the abstract collective good experiences maximum extraction but has no power to resist. The analytical observer (analytical exit) derives d ~0.70, producing f(d) ~1.15 — the observer is not extracted from but sees the structure clearly. The sigmoid f(d) function ensures that beneficiaries with strong exits experience dampened effective extraction (Meta's f(d) < 0), while victims with weak exits experience amplified extraction (grid operators' f(d) > 1.0). No directionality overrides are necessary — the structural derivation produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: Meta's bilateral agreements decompose into at least three structurally distinct constraints: (1) the SMR financing problem (can venture capital + bilateral agreements de-risk nuclear development faster than traditional utility models?), (2) the market consolidation problem (do large bilateral agreements constitute monopolistic harm or efficient matching?), (3) the grid planning problem (do bilateral structures improve or degrade grid reliability?). These are not mere observational variations of a single constraint — they have different ε values, different victim/beneficiary structures, and different regulatory resolutions. SMR financing as a pure coordination problem would be Rope (ε ~0.15–0.25). Market consolidation is Snare or Tangled Rope (ε ~0.45–0.65). Grid planning coordination is Scaffold or Rope (ε ~0.20–0.40). The unified Meta constraint story (presented here) treats them as a single tangled rope because the offtake agreement bundles all three problems together structurally. However, future analysis may separate these stories if empirical work reveals that one component (e.g., SMR financing) decouples from another (e.g., market consolidation). The mandatrophy is resolved by recognizing that the tangled rope classification is the correct synthetic view: the constraint genuinely exhibits both coordination (SMR finance + supply matching) and extraction (market consolidation + grid planning externality). The theater ratio is low (0.35) because the extraction mechanism is not hidden — it operates through visible market structure, not performance or ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_permission_boundary,
    'Will FERC and state regulators permit large-scale bilateral power purchase agreements to bypass wholesale market participation, or will they mandate wholesale participation and regulator-approved cost recovery?',
    'FERC orders, state regulatory decisions, court challenges to merchant generation structures, legislative changes to grid market rules',
    'If regulators permit bilateral bypass: the constraint escalates toward pure snare (market consolidation). If regulators mandate wholesale participation: the constraint degrades toward pure rope (coordination with transparent pricing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_permission_boundary, conceptual, 'Whether regulators allow bilateral offtake agreements to bypass wholesale markets').

omega_variable(
    smr_deployment_timeline_realism,
    'Will SMR vendors achieve cost-competitive, grid-ready deployment on the timeline promised in Meta agreements (2027–2035), or will technical delays and cost overruns force renegotiation?',
    'Comparative analysis of promised vs actual SMR timelines; cost tracking for deployed units; regulatory approval pace (NRC licensing, environmental review); financing realization rates',
    'If timelines hold: bilateral structure locks in extraction for 20+ years. If delays extend 5+ years: extraction opportunity window closes (grid decarbonization via renewables makes bilateral SMR agreements less necessary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smr_deployment_timeline_realism, empirical, 'Whether SMR deployment timelines are achievable').

omega_variable(
    competitive_generator_coalition_formation,
    'Will competitive generators, utilities, and grid operators organize to challenge bilateral agreements through regulatory proceedings, or will they accept market consolidation passively?',
    'FERC complaint filings, state regulatory interventions, legislative lobbying, litigation outcomes, market participation metrics',
    'If coalition forms: regulatory pressure may force market opening (increasing χ in the constraint, raising suppression). If passive acceptance: extraction persists unchallenged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competitive_generator_coalition_formation, preference, 'Whether competitive generators organize political opposition').

omega_variable(
    grid_stability_empirical_impact,
    'Does Meta''s demand flexibility (data center load shifting, geographic arbitrage) actually provide grid services that offset the market consolidation cost, or is demand-side flexibility insufficient to justify the market distortion?',
    'Grid impact studies comparing actual vs promised flexibility; analysis of real-time dispatch data; frequency stability metrics with and without Meta load response',
    'If flexibility is substantial: tangled rope classification confirmed (real coordination benefit + extraction). If flexibility is limited: snare classification escalates (extraction without offsetting coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_empirical_impact, empirical, 'Whether Meta''s demand flexibility provides grid services').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_nuclear_power_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_nuc_tr_t0, meta_nuclear_power_agreement, theater_ratio, 0, 0.5).
narrative_ontology:measurement(meta_nuc_tr_t5, meta_nuclear_power_agreement, theater_ratio, 5, 0.38).
narrative_ontology:measurement(meta_nuc_tr_t10, meta_nuclear_power_agreement, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(meta_nuc_be_t0, meta_nuclear_power_agreement, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(meta_nuc_be_t5, meta_nuclear_power_agreement, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(meta_nuc_be_t10, meta_nuclear_power_agreement, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_nuclear_power_agreement, resource_allocation).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, smr_capital_finance_bottleneck).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, wholesale_electricity_market_consolidation).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, renewable_grid_integration_timing).

% DUAL FORMULATION NOTE:
% Meta's agreements bundle three distinct structural constraints: (1) SMR financing (upstream: capital availability), (2) bilateral market consolidation (lateral: wholesale market dynamics), (3) grid planning coordination (downstream: regional reliability). Each has different ε values and victim structures. The unified constraint story (tangled rope) integrates them because they are structurally coupled by the offtake agreement. If empirical work reveals decoupling (e.g., SMR financing succeeds while market consolidation fails), the constraint family should decompose into separate stories linked by network edges. See 'uke_scope' field for UKE_SCOPE manifest provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meta_nuclear_power_agreement, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
