% ============================================================================
% CONSTRAINT STORY: incumbent_competitive_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_competitive_moat, []).

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
 *   constraint_id: incumbent_competitive_moat
 *   human_readable: Incumbent Competitive Moat in Concentrated Markets
 *   domain: economic/industrial_organization
 *
 * SUMMARY:
 *   An incumbent competitive moat is a structural constraint that protects a
 *   dominant firm's market position through barriers to entry, switching
 *   costs, network effects, and scale advantages. This constraint exhibits a
 *   fundamental tension: moats can represent either genuine coordination
 *   efficiency (economies of scale, network effects that benefit consumers
 *   through standardized services) or extractive rent-seeking (use of market
 *   power to suppress competition and raise prices above marginal cost). The
 *   classification varies dramatically by observer position. The incumbent
 *   experiences the moat as rational competitive advantage (Rope). Excluded
 *   entrants experience it as an insurmountable barrier (Snare). Consumers
 *   experience mixed benefits and extraction (Tangled Rope). Regulators see
 *   it as a temporary structural problem amenable to intervention (Scaffold).
 *   The institutional infrastructure maintains it through inertia (Piton).
 *   Disruptors from adjacent markets see it as partially surmountable
 *   (Tangled Rope). The analytical observer risks naturalizing what is
 *   actually a contingent institutional arrangement as a law of capitalist
 *   competition (Mountain). The constraint's extractiveness has grown from
 *   0.42 to 0.58 over a 20-year interval as incumbent firms have invested in
 *   moat-deepening: exclusive dealing, algorithmic lock-in, data aggregation,
 *   and regulatory capture. Theater has remained relatively stable (0.25 →
 *   0.38) because moats are genuinely effective coordination mechanisms — the
 *   extraction is not purely performative.
 *
 * KEY AGENTS:
 *   - Incumbent Firm: Primary beneficiary (institutional/arbitrage) — captures monopoly rents, controls supply channels, sets industry standards; can exit through acquisition, licensing, or market pivot without losing competitive position
 *   - Potential Entrants: Primary victims (powerless/trapped) — face sunk cost barriers, network effects, brand switching costs, and incumbent retaliation; cannot overcome moat barriers through normal competition
 *   - Consumer Welfare: Secondary victim (moderate/constrained) — benefits from incumbent's scale and coordination but subject to price extraction above marginal cost; have partial exit options (switching to substitutes, delayed purchases) at surmountable but significant costs
 *   - Regulatory Oversight Coalition: Organized actors (organized/constrained) — antitrust enforcers, standard-setting bodies, can intervene through merger review, compulsory licensing, interoperability mandates; have agency and see exit paths through policy
 *   - Adjacent Market Disruptors: Powerful entrants (powerful/mobile) — platform companies, emerging-technology firms, international competitors from different sectors; can leverage alternative business models and user bases to partially erode moat; have mobile exit options if initial challenge fails
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of capitalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_competitive_moat, 0.58).
domain_priors:suppression_score(incumbent_competitive_moat, 0.52).
domain_priors:theater_ratio(incumbent_competitive_moat, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_competitive_moat, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_competitive_moat, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(incumbent_competitive_moat, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_competitive_moat, tangled_rope).
narrative_ontology:human_readable(incumbent_competitive_moat, "Incumbent Competitive Moat in Concentrated Markets").
narrative_ontology:topic_domain(incumbent_competitive_moat, "economic/industrial_organization").

domain_priors:requires_active_enforcement(incumbent_competitive_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_competitive_moat, incumbent_firm).
narrative_ontology:constraint_victim(incumbent_competitive_moat, potential_entrants).
narrative_ontology:constraint_victim(incumbent_competitive_moat, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED COMPETITOR (SNARE) — New entrants face sunk cost barriers (capital requirements, network effects, supplier lock-in, regulatory compliance), brand-switching costs, and incumbent retaliation strategies (predatory pricing, exclusive dealing). Cannot exit the market structure itself; trapped by the structural barriers that make entry infeasible. Maximum extraction from this position — the moat directly redistributes profit from potential entrants to the incumbent.
constraint_indexing:constraint_classification(incumbent_competitive_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBSTITUTION-SEEKING CONSUMER (TANGLED ROPE) — Consumers benefit from the incumbent's scale efficiencies and coordination of supply (reliable availability, standardized service). Simultaneously, the moat restricts substitution options and enables price extraction above marginal cost. Exit costs vary (switching platforms, learning new products) but are surmountable — consumers have partial agency. Mixed extraction and coordination.
constraint_indexing:constraint_classification(incumbent_competitive_moat, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — The moat solves a genuine coordination problem: economies of scale, network effects, and supplier relationships require sustained integration and standardization. The incumbent experiences the moat as a coordination mechanism that enables efficient production and reliable market function. Net beneficiary; can exit through arbitrage (licensing, divesting segments, pivoting to adjacent markets) without losing core value. The constraint appears as rational competitive advantage rather than extraction.
constraint_indexing:constraint_classification(incumbent_competitive_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY OVERSIGHT COALITION (SCAFFOLD) — Antitrust enforcers and standard-setting bodies see the moat as a temporary structural problem with a sunset clause: merger review, compulsory licensing, open-access mandates, and interoperability requirements are building alternative coordination pathways. Regulatory intervention can degrade the moat without destroying underlying coordination value. Organized agents (DOJ, FTC, EU DG Comp) have agency and see exit routes through policy. Low effective extraction because intervention has real force.
constraint_indexing:constraint_classification(incumbent_competitive_moat, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY MARKET STRUCTURE (PITON) — Industrial organization theory identifies moats as natural and efficient features of capitalism. Yet many moats persist primarily through behavioral lock-in, regulatory capture, and institutional inertia rather than genuine economic advantage. The moat's theater (merger-and-acquisition narratives, innovation claims, competitive posturing) exceeds its functional content for many mature incumbents. Market participants maintain the structure through habit and expectation, not because alternatives have been exhausted. Piton classification derives from the theater gate (0.38) and the inertial maintenance of the structure despite partial functional degradation.
constraint_indexing:constraint_classification(incumbent_competitive_moat, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ADJACENT DISRUPTOR (TANGLED ROPE) — New entrants from adjacent markets (digital disruption, platform competition, emerging geographies) experience the moat as a mixed constraint. They benefit from the incumbent's infrastructure investments and market education but must overcome switching costs and network effects. They have greater agency than excluded competitors (mobile exit options — they can exit by pivoting to other markets or building parallel platforms) but still face significant extraction through margin compression and exclusionary practices. Moderate extraction, genuine coordination benefit.
constraint_indexing:constraint_classification(incumbent_competitive_moat, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical view, competitive moats appear as natural features of capitalism itself: scale advantages, learning curves, and network effects are inherent to market processes. This perspective naturalizes the moat as an immutable law of competitive dynamics. However, the structural data reveals this as a false summit — moats are contingent on property rules, regulatory frameworks, and institutional arrangements, not on laws of physics or logic. The mountain classification is perspectival naturalization of what is actually a Tangled Rope hybrid.
constraint_indexing:constraint_classification(incumbent_competitive_moat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_competitive_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_competitive_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_competitive_moat, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_competitive_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_competitive_moat, TR),
    TR >= 0.70.

:- end_tests(incumbent_competitive_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The incumbent captures significant rents above marginal cost through the moat, redistributing surplus from consumers and excluded entrants to equity holders. The value (0.58 rather than 0.75) reflects that moats contain genuine coordination benefits — economies of scale, network effects, standardization — that justify partial extraction. However, the growth trajectory from 0.42 to 0.58 suggests accumulating extractive behavior (exclusive dealing, data aggregation, algorithmic lock-in) beyond what baseline scale advantages require. Suppression (0.52): Moderate-high. Barriers to entry include capital requirements (network infrastructure, platform buildout), switching costs (user base lock-in, data portability limits), incumbent retaliation (predatory pricing, exclusive supplier agreements), and regulatory barriers (licensing, spectrum allocation). Suppression is not total (disruptors do occasionally overcome moats; antitrust action is possible) but substantial enough to trap potential entrants. Theater ratio (0.38): Low-moderate. Moats are functionally real — economies of scale and network effects exist — but the narrative exceeds the structural reality. Incumbent marketing ('innovation,' 'ecosystem,' 'natural monopoly') inflates the perceived legitimacy of the moat. Theater has grown as moats have deepened, suggesting increasing reliance on narrative to justify extraction. This Tangled Rope classification reflects both coordination function (the moat does solve real problems) and extraction asymmetry (rents are concentrated).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the incumbent's Rope and the excluded competitor's Snare is the widest in this constraint story. For the incumbent, the moat solves genuine problems (coordination, scale, standardization). For the excluded competitor, the moat is insurmountable. This gap reveals the constraint's hybrid nature: the coordination function is real, but it is asymmetrically distributed — coordination benefits flow to the incumbent and existing consumers, while excluded competitors are shut out entirely. The Scaffold perspective (regulatory intervention) represents a real structural feature: policy tools exist to degrade moats (compulsory licensing, interoperability mandates, merger review) without destroying underlying coordination value. The Adjacent Disruptor perspective (Tangled Rope from a powerful/mobile position) is empirically important: disruption is the primary mechanism through which moats erode in practice, suggesting that the Scaffold sunset is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: incumbent_firm (receives monopoly rents, controls market structure). Victims: potential_entrants (blocked from competition), consumer_welfare (pays prices above marginal cost). The incumbent's directionality (d ≈ 0.15, institutional/arbitrage) produces low χ: they are the beneficiary and can exit freely. Excluded entrants' directionality (d ≈ 0.95, powerless/trapped) produces high χ: they are victims with no exit. Consumers' directionality (d ≈ 0.55, moderate/constrained) produces mid-range χ: they are partly victimized, partly benefited, with constrained exit. The regulatory coalition's directionality (d ≈ 0.50, organized/constrained) reflects that they are neither victims nor beneficiaries but have agency — organized power allows some intervention despite political constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The question is not 'is this Rope or Snare?' but 'for whom?' The incumbent correctly perceives Rope — the moat does coordinate supply, enable scale, and solve real problems. The excluded competitor correctly perceives Snare — the moat prevents entry through insurmountable barriers. The analytical observer who sees 'natural competitive advantage' (Mountain) is naturalizing a contingent institutional arrangement — scale advantages and network effects are real, but the degree to which these translate to extractive monopoly rents is a policy choice, not a natural law. The Scaffold perspective (regulatory intervention can degrade moats without destroying coordination) is empirically grounded: licensing, interoperability mandates, and merger review do reduce moat strength while preserving beneficial scale and standardization. The mandatrophy is resolved by recognizing that the constraint contains genuine coordination (Rope) plus genuine extraction (Snare) simultaneously, distributed asymmetrically across observers. The Tangled Rope classification at the analytical level correctly captures this hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moat_legitimacy_threshold,
    'What share of incumbent advantage derives from genuine economies of scale vs. extractive barriers and regulatory protection?',
    'Comparative analysis of incumbent margins vs. theoretical marginal-cost pricing; counterfactual scenario modeling of open-access markets (fiber-to-the-home, mandatory interoperability, patent pool); cross-country regulatory variation in moat structural integrity',
    'If legitimate advantages > 70%: moat is primarily Rope. If extractive barriers > 50%: moat is primarily Snare. If split: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moat_legitimacy_threshold, empirical, 'Proportion of incumbent advantage from scale vs. extraction').

omega_variable(
    network_effect_necessity,
    'Are network effects (switching costs, interoperability lock-in) inherent to the service or engineered to lock in users?',
    'Analysis of counterfactual interoperability scenarios; examination of switching-cost reduction through API standardization or data portability; cross-platform compatibility experiments',
    'If inherent: network effects legitimize partial moat extraction (coordination benefit). If engineered: extraction mechanism dominates, strengthening Snare classification for entrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_necessity, empirical, 'Whether network effects are inherent or engineered').

omega_variable(
    regulatory_capture_mechanism,
    'How much of the moat''s persistence derives from direct regulatory protection (licensing, entry barriers, exclusive dealing laws) vs. market structure alone?',
    'Decomposition of moat strength with and without regulatory restrictions; analysis of historical deregulation episodes and moat erosion; regulatory lobbying expenditure correlation with moat stability',
    'If regulatory protection < 20%: moat is primarily market-based coordination. If regulatory protection > 50%: moat depends on state enforcement, strengthening Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Regulatory protection contribution to moat strength').

omega_variable(
    disruptive_entry_feasibility,
    'Are adjacent-market entrants genuinely capable of eroding the incumbent''s moat (digital platforms, emerging geographies, substitute technologies), or are these threat narratives without real structural force?',
    'Historical analysis of successful moat erosion events; tracking of new-entrant success rates in platform and infrastructure markets; measurement of disruptive technology adoption curves',
    'If feasible disruption > 40%: scaffold sunset is real, and regulatory intervention may be unnecessary. If disruption < 20%: moat is more durable than narrative suggests, strengthening Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disruptive_entry_feasibility, empirical, 'Feasibility of disruptive entry to erode moat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_competitive_moat, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moat_tr_t0, incumbent_competitive_moat, theater_ratio, 0, 0.25).
narrative_ontology:measurement(moat_tr_t10, incumbent_competitive_moat, theater_ratio, 10, 0.32).
narrative_ontology:measurement(moat_tr_t20, incumbent_competitive_moat, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(moat_be_t0, incumbent_competitive_moat, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(moat_be_t10, incumbent_competitive_moat, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(moat_be_t20, incumbent_competitive_moat, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_competitive_moat, resource_allocation).
narrative_ontology:affects_constraint(incumbent_competitive_moat, platform_network_effects).
narrative_ontology:affects_constraint(incumbent_competitive_moat, regulatory_capture_economic).
narrative_ontology:affects_constraint(incumbent_competitive_moat, predatory_pricing_mechanism).

% DUAL FORMULATION NOTE:
% Incumbent competitive moat is upstream of specific extractive mechanisms (predatory pricing, exclusive dealing, network lock-in) and downstream of market structure features (scale economies, switching costs). The moat story serves as the integrative frame; the linked constraints represent specific manifestations of how moats are built and maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incumbent_competitive_moat, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
