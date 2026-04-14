% ============================================================================
% CONSTRAINT STORY: sk_dantongbeop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_dantongbeop, []).

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
 *   constraint_id: sk_dantongbeop
 *   human_readable: South Korea's Mobile Device Distribution Improvement Act (Dantongbeop)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   South Korea's Dantongbeop (Mobile Device Distribution Improvement Act),
 *   enacted in 2014, was officially framed as stabilizing the mobile device
 *   market through transparent and non-discriminatory subsidy allocation.
 *   However, the constraint exhibits structural characteristics of a tangled
 *   hybrid: the law provides genuine coordination benefits (standardized
 *   pricing disclosures, procedural predictability) while simultaneously
 *   entrenching asymmetric extraction through carrier control of subsidy
 *   flows. The law appears to address market failures (predatory subsidy
 *   practices by carriers) but the underlying oligopolistic structure—SK
 *   Telecom, KT, and LG U+ control 95%+ of the market—persists unchanged.
 *   Dantongbeop's theater ratio has increased over its implementation period
 *   as regulatory compliance became decoupled from competitive pressure.
 *   Small retailers face trapped exit options within the subsidy system,
 *   while major carriers benefit from the law's procedural legitimacy without
 *   surrendering subsidy allocation authority. The constraint demonstrates
 *   how transparency requirements can constitute coordination theater when
 *   underlying structural asymmetries (market concentration, control over
 *   network infrastructure) remain unaddressed.
 *
 * KEY AGENTS:
 *   - Major Carriers (SK Telecom, KT, LG U+): Primary beneficiaries (institutional/arbitrage) — control subsidy allocation, set device pricing boundaries, capture market stabilization benefits
 *   - Small Mobile Retailers: Primary victims (powerless/trapped) — dependent on carrier subsidy levels, cannot exit without abandoning retail operations, face margin compression
 *   - MVNO Operators and Regional Carriers: Secondary victims (moderate/constrained) — excluded from subsidized device supply chains, must compete on prepaid models at higher customer acquisition cost
 *   - Device Manufacturers: Secondary beneficiaries (institutional/arbitrage) — stabilized demand via subsidy system, reduced pressure for direct distribution, protected from price competition
 *   - Consumer Advocacy Groups: Organized agents (organized/constrained) — benefit from transparency requirements but lack leverage to restructure underlying subsidy asymmetry
 *   - South Korean Regulatory Apparatus (KCC, FTC): Institutional actor (institutional/arbitrage) — maintains compliance oversight; benefits from enforcement legitimacy; maintains Dantongbeop despite evidence of continued extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing oligopolistic market structure as immutable, conflating structural economics with spectrum/merger policy contingencies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_dantongbeop, 0.52).
domain_priors:suppression_score(sk_dantongbeop, 0.65).
domain_priors:theater_ratio(sk_dantongbeop, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_dantongbeop, extractiveness, 0.52).
narrative_ontology:constraint_metric(sk_dantongbeop, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sk_dantongbeop, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_dantongbeop, tangled_rope).
narrative_ontology:human_readable(sk_dantongbeop, "South Korea's Mobile Device Distribution Improvement Act (Dantongbeop)").
narrative_ontology:topic_domain(sk_dantongbeop, "economic/technological").

domain_priors:requires_active_enforcement(sk_dantongbeop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_dantongbeop, major_carriers).
narrative_ontology:constraint_beneficiary(sk_dantongbeop, device_manufacturers).
narrative_ontology:constraint_victim(sk_dantongbeop, small_retailers).
narrative_ontology:constraint_victim(sk_dantongbeop, consumer_choice).
narrative_ontology:constraint_victim(sk_dantongbeop, mvno_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MOBILE RETAILERS (SNARE) — Trapped in a market structure where subsidy allocation is controlled by major carriers. Cannot exit without abandoning retail operations. Subject to carrier-imposed conditions on device pricing, inventory, and promotional flexibility. Maximum suppression — carriers set subsidy levels unilaterally, retailers absorb margin pressure.
constraint_indexing:constraint_classification(sk_dantongbeop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MVNO OPERATORS & REGIONAL CARRIERS (TANGLED ROPE) — Constrained by lack of subsidized device access and customer acquisition costs, but benefit from device price transparency requirements. Face extraction through subsidy asymmetry but also gain coordination benefit from standardized pricing disclosures. Can partially exit via prepaid models but at higher customer acquisition cost.
constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAJOR CARRIERS (ROPE) — Net beneficiaries of subsidy control mechanism. Experience Dantongbeop as coordination infrastructure that legitimizes their subsidy allocation practices while appearing market-compliant. Can arbitrage between consumer segments by varying subsidy levels within regulatory bounds. Active enforcement of subsidy caps provides cover for cartel-like coordination.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEVICE MANUFACTURERS (ROPE) — Benefit from subsidized device ecosystem that stabilizes demand and insulates consumer prices from competition. Dantongbeop enforces wholesale supply relationships with major carriers, reducing pressure for direct-to-consumer channels. Can arbitrage geographic pricing and manufacturing location decisions within regulatory framework.
constraint_indexing:constraint_classification(sk_dantongbeop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER ADVOCACY & CIVIC OVERSIGHT (TANGLED ROPE) — Organized agents (consumer groups, civil society oversight bodies) see mixed structure: transparency requirements provide coordination benefit (accessible price information, standardized disclosures) but effective extraction persists through subsidy asymmetry and lock-in mechanisms. Constrained by limited leverage over carrier oligopoly; can pressure for rule clarification but cannot force subsidy redistribution.
constraint_indexing:constraint_classification(sk_dantongbeop, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY APPARATUS (PITON) — Maintains Dantongbeop through institutional inertia despite degraded effectiveness. Original function (prevent predatory subsidy practices) has atrophied; law now serves primarily as procedural theater—mandatory reporting and transparency without price-control teeth. Theater ratio (0.58) reflects that compliance is largely formal; real subsidy allocation remains carrier-discretionary within transparent bounds. Regulation persists because dismantling it would admit failure.
constraint_indexing:constraint_classification(sk_dantongbeop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER—NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, market concentration in telecommunications creates inevitable subsidy asymmetries: oligopolistic carriers with customer lock-in must extract rents from dependent retailers. The structural economics produce the observed pattern regardless of regulation. Dantongbeop appears as a futile intervention against natural market consolidation. However, this perspective risks naturalizing what are contingent institutional choices (spectrum allocation, merger approval, regulatory forbearance).
constraint_indexing:constraint_classification(sk_dantongbeop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_dantongbeop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_dantongbeop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_dantongbeop, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_dantongbeop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_dantongbeop, TR),
    TR >= 0.70.

:- end_tests(sk_dantongbeop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Carriers capture rents through subsidy control—they set device wholesale prices, subsidy levels, and promotional bounds. Small retailers face margin compression and inventory constraints imposed unilaterally. However, the extraction is not total (Snare-level 0.66+) because the law's transparency requirements do create some coordination value and procedural predictability. Subsidy asymmetry is measurable but not absolute—retailers retain some pricing agency within carrier-set bounds. Suppression (0.65): High. Barriers to independent device sales include: (1) carrier lock-in via subsidized pricing (2) spectrum duopoly preventing MVNO device subsidies (3) cultural expectation of carrier-subsidized devices (4) regulatory authorization for carrier subsidy control. Retailers cannot exit to unsubsidized distribution without losing customer base. Theater ratio (0.58): Moderate. Dantongbeop's transparency requirements are genuine—mandatory subsidy disclosures, standardized reporting, procedural consistency. However, theater has increased over time because compliance became decoupled from competitive pressure. Carriers publish subsidy levels but retain discretionary allocation. Theater reflects that procedure is visible while economic extraction continues.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits substantial perspectival disagreement. Beneficiary carriers see pure coordination (Rope); trapped retailers see pure extraction (Snare boundary); regulatory analysts see performative degradation (Piton); organized consumers see mixed extraction masked by transparency (Tangled Rope). The gap reveals that official framing (stabilization, non-discrimination) has decoupled from structural effect (oligopoly-managed subsidy control). Unlike the verification bottleneck exemplar, this constraint does NOT resolve into a coherent six-type presheaf—instead it exhibits what we call 'narrative collapse,' where the official story (transparent market stabilization) contradicts the agent observations (continued extraction under transparent rules). This indicates that the constraint's classification is contaminated by false natural law framing. The analytical observer's mountain perspective is a false summit: treating oligopolistic telecommunications as natural law when it is the product of spectrum allocation and merger approval decisions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to subsidy extraction. Major carriers: beneficiaries with arbitrage exit (d ≈ 0.10-0.15) → low f(d) → minimal experienced extraction. Small retailers: victims with trapped exit (d ≈ 0.90-0.95) → high f(d) ≈ 1.35-1.42 → maximum experienced extraction. MVNOs: victims with constrained exit (d ≈ 0.75-0.80) → high f(d) ≈ 1.10-1.20 → high experienced extraction but not maximal (some agency via prepaid models). Regulatory apparatus: institutional beneficiaries with arbitrage exit (d ≈ 0.10) → negative f(d) ≈ -0.12 → regulation benefits compliance visibility. Transparency requirements feed the derivation chain as a low-extraction coordination feature, but the underlying subsidy asymmetry produces high d for trapped agents. The piton classification emerges from theater_ratio (0.58) crossing 0.50 combined with enforcement that no longer produces behavioral change.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED. The constraint's extractiveness (0.52) falls below the 0.70 threshold for forced resolution, but the perspectival gap suggests latent mandatrophy. The ambiguity is NOT between 'pure extraction' (Snare) and 'pure coordination' (Rope)—both types appear in different perspectives. Instead, the mandatrophy concerns what 'coordination' means in the presence of suppressed alternatives. Dantongbeop provides genuine coordination benefits (transparent pricing, predictable subsidy rules) but coordinates market participants in a structure where some participants (small retailers) have no exit option. This is coordination OF extraction, not coordination with extraction. To fully resolve: (1) Determine whether alternative distribution models (unsubsidized, direct-to-consumer, MVNO-subsidized) are economically viable or structurally blocked by Dantongbeop. If blocked: constraint is Snare disguised as Rope. (2) Determine whether transparency reduced or legalized asymmetry. If legalized: theater_ratio should rise further and classification should migrate toward Piton. (3) Test whether oligopoly concentration is natural or policy-contingent. If policy-contingent: the constraint is not mountain (natural law) but rather a choice to stabilize a chosen market structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_allocation_mechanism,
    'Is subsidy allocation authority inherent to carrier network economics (natural monopoly justification) or contingent on historical spectrum/merger policy decisions?',
    'Comparative analysis of subsidy models across countries with different spectrum allocation and merger policies; decomposition of regulatory justifications vs. actual economic necessity',
    'If inherent: oligopoly-driven extraction is natural law, regulation can only mitigate, Dantongbeop is appropriate scaffold. If contingent: subsidy control is a designed extraction mechanism that could be restructured (e.g., universal service fund, device portability, customer switching). Changes classification from Mountain to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_allocation_mechanism, conceptual, 'Whether carrier subsidy control is economically inherent or policy-contingent').

omega_variable(
    transparency_effectiveness,
    'Does mandatory subsidy transparency in Dantongbeop actually reduce extraction or merely legalize it by making practices visible?',
    'Empirical comparison: subsidy asymmetry and retail margin compression before/after 2014 enactment; cross-country price transparency vs. competition outcomes; analysis of whether transparency led to policy changes or only to legitimated disclosure',
    'If effective: transparency reduced asymmetry, Dantongbeop is functional Rope. If theater: visibility was decoupled from behavioral change, Dantongbeop is Piton with higher theater ratio. Could shift classification from Tangled Rope toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_effectiveness, empirical, 'Whether transparency requirements actually constrain subsidy extraction').

omega_variable(
    alternative_distribution_viability,
    'Could direct-to-consumer sales, bundled carrier-device offerings without subsidy intermediaries, or device financing platforms provide functionally equivalent consumer access without carrier control of subsidy allocation?',
    'Market data on unsubsidized device sales penetration; case studies of markets with reduced carrier subsidy role (EU device unlocking mandates, US CDMA/GSM device compatibility); cost-benefit analysis of alternative customer acquisition channels',
    'If viable alternatives exist: subsidy dependency is artificially maintained, Dantongbeop enables extraction by blocking alternatives. If alternatives are unviable: subsidy system is coordination necessity, Dantongbeop is appropriate stabilization. Changes whether constraint is Snare (alternatives blocked) vs. Rope (necessary coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_distribution_viability, empirical, 'Whether carrier-independent device distribution could supplant subsidized sales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_dantongbeop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sk_d_tr_t0, sk_dantongbeop, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sk_d_tr_t5, sk_dantongbeop, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sk_d_tr_t10, sk_dantongbeop, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sk_d_be_t0, sk_dantongbeop, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sk_d_be_t5, sk_dantongbeop, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sk_d_be_t10, sk_dantongbeop, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_dantongbeop, resource_allocation).
narrative_ontology:affects_constraint(sk_dantongbeop, south_korean_telecom_oligopoly).
narrative_ontology:affects_constraint(sk_dantongbeop, device_manufacturer_distribution_models).

% DUAL FORMULATION NOTE:
% Dantongbeop decomposes into two structural claims: (1) transparency of subsidy allocation (low-extractiveness coordination, ε ≈ 0.15, Rope); (2) carrier control of subsidy flows given oligopolistic market structure (high-extractiveness control, ε ≈ 0.68, Snare). The unified story reports ε = 0.52 as a blend, but the two claims have different empirical status and different classification outcomes. The transparency claim (ε = 0.15) is well-established; the control claim (ε = 0.68) remains contested in regulatory and academic literature. These should be modeled as separate constraints linked via network.affects_constraints if full decomposition is desired.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
