% ============================================================================
% CONSTRAINT STORY: spectrum_allocation_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spectrum_allocation_monopoly, []).

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
 *   constraint_id: spectrum_allocation_monopoly
 *   human_readable: Spectrum Allocation Monopoly in Wireless Communications
 *   domain: regulatory/telecommunications/economic
 *
 * SUMMARY:
 *   Spectrum allocation monopoly represents a structural constraint where
 *   regulatory gatekeepers control access to a finite but partially
 *   artificially-scarce resource—the radio frequency spectrum. The constraint
 *   exhibits high extractiveness (0.68) because incumbent carriers capture
 *   enormous economic rents through exclusive spectrum allocations, while new
 *   entrants face functionally insurmountable barriers to market entry.
 *   Suppression is particularly high (0.72) because spectrum access is
 *   fundamentally regulated: there is no black market, no workaround, and no
 *   secondary mechanism for acquiring spectrum allocation rights outside the
 *   official regulatory process. The theater ratio (0.45) is moderate because
 *   the constraint maintains genuine technical function (preventing radio
 *   interference) while simultaneously serving extractive incumbency
 *   protection. Over the 30-year measurement interval, extractiveness has
 *   increased from 0.45 to 0.68 as the value of spectrum has grown with
 *   wireless technology adoption, while theater ratio has declined from 0.55
 *   to 0.45—indicating that the technical justification for spectrum
 *   allocation has weakened (modern technologies like cognitive radio and
 *   unlicensed band sharing make command-and-control allocation increasingly
 *   obsolete), yet the extraction mechanism persists and strengthens.
 *
 * KEY AGENTS:
 *   - New Market Entrants: Primary victims (powerless/trapped) — face multi-billion-dollar barriers to spectrum access; functionally excluded from wireless markets
 *   - Spectrum-Dependent Innovation Pathways: Secondary victims (powerless/trapped) — entire technology development streams (IoT, autonomous systems, next-gen cellular) blocked by artificial scarcity
 *   - Incumbent Carriers: Primary beneficiaries (institutional/arbitrage) — capture economic rents from exclusive spectrum allocations; experience constraint as coordination mechanism
 *   - Regulatory Agencies: Secondary beneficiary (institutional/constrained) — capture auction revenue; maintain control over market structure; experience constraint as both coordination and enforcement mechanism
 *   - Command-and-Control Allocation Model: Institutional arrangement (institutional/arbitrage) — technically obsolete but maintained through regulatory inertia (piton classification)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes constraint as hybrid coordination-extraction with growing extractive component as technological alternatives emerge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spectrum_allocation_monopoly, 0.68).
domain_priors:suppression_score(spectrum_allocation_monopoly, 0.72).
domain_priors:theater_ratio(spectrum_allocation_monopoly, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spectrum_allocation_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(spectrum_allocation_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(spectrum_allocation_monopoly, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spectrum_allocation_monopoly, snare).
narrative_ontology:human_readable(spectrum_allocation_monopoly, "Spectrum Allocation Monopoly in Wireless Communications").
narrative_ontology:topic_domain(spectrum_allocation_monopoly, "regulatory/telecommunications/economic").

domain_priors:requires_active_enforcement(spectrum_allocation_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spectrum_allocation_monopoly, incumbent_carriers).
narrative_ontology:constraint_beneficiary(spectrum_allocation_monopoly, regulatory_agencies).
narrative_ontology:constraint_victim(spectrum_allocation_monopoly, spectrum_access_seekers).
narrative_ontology:constraint_victim(spectrum_allocation_monopoly, consumer_innovation_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANTS (SNARE) — Face absolute barriers to spectrum access. Licensing costs exceed billions; no secondary market permits real-time frequency trading. Entry into wireless markets is functionally impossible without spectrum allocation approval from regulatory gatekeepers. Maximum suppression and extraction with zero exit options.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SPECTRUM-DEPENDENT INNOVATION (SNARE) — Entire technology development pathways (IoT, autonomous vehicles, next-generation cellular) are blocked by spectrum scarcity that is partly artificial. The constraint is generational because decades of missed innovation cannot be recovered. Technologies requiring spectrum access but lacking incumbent backing face extinction.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT CARRIERS (ROPE) — Benefit from exclusive spectrum allocations. Experience the constraint as coordination: managing interference between licensed users, coordinating spectrum-sharing standards, communicating technical requirements to regulators. Net beneficiaries with full exit optionality (can divest, merge, or operate in alternative markets). The constraint subsidizes their market position.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Manages the allocation mechanism through auctions and licensing. Experiences genuine coordination function (preventing interference, assigning frequencies efficiently) alongside asymmetric extraction (agency captures auction revenue, controls market structure through allocation decisions). Active enforcement required; cannot exit without legislative restructuring.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMAND-AND-CONTROL MODEL (PITON) — The original allocation mechanism (regulatory assignment of specific bands to specific users) was designed for radio scarcity in 1930-1950s. Modern technology enables dynamic spectrum sharing, cognitive radio, and real-time frequency negotiation. The command-and-control model persists through regulatory inertia despite being technically obsolete. Theater ratio is moderate because the model still performs its assigned function (preventing interference) but does so through increasingly theatrical scarcity assertions rather than genuine technical limits.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, spectrum allocation serves real coordination functions (interference prevention, band standardization) while simultaneously enabling rent extraction. The constraint is not a mountain: spectrum scarcity is partly artificial—driven by regulatory allocation choices rather than physics. Cognitive radio and dynamic spectrum access technologies could reduce artificial scarcity, but implementation is blocked by incumbent incumbency and regulatory path dependence. The asymmetric extraction of benefit toward incumbents is the operative mechanism.
constraint_indexing:constraint_classification(spectrum_allocation_monopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spectrum_allocation_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spectrum_allocation_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spectrum_allocation_monopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spectrum_allocation_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spectrum_allocation_monopoly, TR),
    TR >= 0.70.

:- end_tests(spectrum_allocation_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Incumbency carriers enjoy monopolistic pricing power within allocated spectrum bands; entry barriers prevent competitive discipline. The measure reflects that new entrants face an effective license requirement costing 5-50 billion USD for viable national coverage, with no assurance of approval. Over 30 years, extractiveness grew from 0.45 to 0.68 as spectrum became more economically valuable and allocation scarcity tightened, but the growth flattened after year 20 as regulations began opening unlicensed bands (WiFi, Bluetooth) providing alternatives for some applications. Suppression (0.72): Very high. Spectrum allocation is a regulated monopoly with no secondary market, no black market alternatives, and no exit other than complete market withdrawal. Unlike capital markets (where barriers to entry exist but can be overcome with sufficient funding), spectrum access cannot be obtained through side channels. This produces the high suppression characteristic of snare classifications. Theater ratio (0.45): Moderate-low. The constraint maintains genuine technical function—radio interference prevention requires coordination, and licensed bands genuinely reduce harmful interference. However, this legitimate function is undermined by the fact that (1) unlicensed bands and modern spectrum-sharing technologies demonstrate that much coordination could occur through technical standards rather than regulatory allocation, (2) the allocation process itself has become increasingly performative (auctions generating revenue rather than optimizing allocation), and (3) command-and-control assignment is technically obsolete relative to cognitive radio alternatives. Theater declined from 0.55 to 0.45 over the interval as technological alternatives (dynamic spectrum access, unlicensed band success) made the original regulatory justification increasingly untenable, yet the constraint strengthened extractively. This pattern—declining theater with rising extractiveness—is characteristic of constraints where the original coordination function has been replaced by rent-seeking through regulatory capture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival gap between incumbents and entrants. Incumbent carriers perceive the constraint as pure coordination (Rope perspective): they are solving the real problem of radio interference and standardization, and the exclusive allocations are a proportionate incentive for infrastructure investment. New entrants perceive the constraint as pure extraction (Snare perspective): they face absolute barriers to market entry, preventing any competitive pressure on pricing or innovation. The regulatory agency occupies an intermediate position (Tangled Rope): it performs legitimate coordination (spectrum conflict prevention, auction administration) while simultaneously extracting rent (through auction revenue and control over market structure) and serving incumbent interests (through allocation decisions that favor incumbents). The analytical observer recognizes that all these perspectives are partially correct—the constraint does coordinate interference prevention, but it also extracts, and the extractive component has grown as technological alternatives have emerged. The piton classification of the command-and-control model itself reflects that this once-necessary institutional arrangement is maintained through inertia despite being technically obsolete.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d derives from the agent's structural relationship to spectrum allocation. Incumbent carriers are beneficiaries with arbitrage options (can divest, merge, operate internationally)—they experience low d (beneficiary modifier reduces directionality) and thus low/negative effective extraction χ. The regulatory agency is a beneficiary (captures auction revenue) with constrained exit (cannot abandon allocation authority without legislative restructuring)—moderate d reflecting mixed extraction and coordination. New entrants are victims (excluded from spectrum access) with trapped exit (no alternative to regulatory approval)—they experience maximum d and thus maximum effective extraction χ. The divergence between incumbent and entrant perspectives is therefore structurally determined: the same constraint produces opposite classifications because the agents occupy opposite positions in the extraction flow. Suppression applies uniformly across all perspectives because spectrum scarcity is a hard constraint (regulatory monopoly on allocation)—no agent can simply acquire spectrum outside official channels regardless of their power level.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES VIA EXTRACTION ASYMMETRY: The mandatrophy is resolved by the asymmetric victim/beneficiary structure. The constraint genuinely coordinates interference prevention (beneficiary perspective: Rope) while genuinely extracting from excluded competitors (victim perspective: Snare). The tangled_rope classifications (regulatory agency, analytical observer) confirm the hybrid structure. The mandatrophy would be unresolved if the constraint were purely extraction (snare-only) or purely coordination (rope-only) with no agent perceiving mixed function. But here, the inconsistency between perspectives is not due to measurement ambiguity—it is due to structural position. Incumbents and entrants genuinely experience opposite constraint types because they occupy opposite positions relative to the extraction mechanism. The analytical observer recognizes this as the constraint's true structure: it is a snare for those excluded, a rope for those included, and a mechanism for incumbent rent extraction regardless of perceived coordination benefit. The rising extractiveness over 30 years (0.45→0.68) with declining theater (0.55→0.45) confirms that the coordination function is increasingly secondary to the extraction function as technological alternatives emerge and the original scarcity justification weakens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_vs_physical_scarcity,
    'How much of observed spectrum scarcity is physical (inherent to radio propagation) versus artificial (regulatory allocation choices)?',
    'Technical analysis of spectrum utilization efficiency; deployment of dynamic spectrum access technologies in controlled environments; international comparison of allocation schemes and resulting market structure',
    'If predominantly physical: constraint should be reclassified as mountain (unavoidable law of physics). If predominantly artificial: classification as snare/tangled_rope confirmed — incumbency protection is the operative mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(artificial_vs_physical_scarcity, empirical, 'Proportion of scarcity that is physical versus regulatory').

omega_variable(
    cognitive_radio_deployment_viability,
    'Can cognitive radio and dynamic spectrum sharing technologies achieve practical deployment at scale, or do they face insurmountable technical obstacles?',
    'Real-world cognitive radio pilots; engineering analysis of interference mitigation reliability; measurement of spectrum utilization efficiency gains',
    'If viable: the piton classification is correct — command-and-control is technically obsolete and maintained by inertia. If not viable: spectrum scarcity enforcement becomes a genuine technical requirement, supporting the natural law argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_radio_deployment_viability, empirical, 'Technical viability of dynamic spectrum access at scale').

omega_variable(
    incumbent_investment_lock_in,
    'To what extent does incumbent infrastructure investment (base stations, device compatibility, spectrum licensing) create irreversible path dependence that justifies regulatory protection?',
    'Economic analysis of sunk cost recovery timelines; comparison to other infrastructure-heavy industries (power grids, transportation networks) and their regulatory treatment; assessment of stranded asset risk if allocation schemes change',
    'If lock-in is severe: tangled_rope classification justified (genuine coordination cost + legitimate protection). If lock-in is manageable: classification tilts toward snare — incumbency protection is rent extraction rather than necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_investment_lock_in, empirical, 'Degree of irreversible infrastructure lock-in').

omega_variable(
    spectrum_auction_design_alternatives,
    'Would alternative auction designs (continuous markets, spectrum trading, dynamic allocation mechanisms) materially reduce extraction compared to current static allocation auctions?',
    'Auction theory analysis; comparison to spectrum trading pilots (US 600MHz incentive auctions, Australia unlicensed band experiments); economic modeling of market efficiency gains',
    'If alternatives are superior: current auction design is a choice supporting incumbent extraction. If alternatives perform worse or equivalently: auction design is defensible coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spectrum_auction_design_alternatives, empirical, 'Whether alternative auction mechanisms reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spectrum_allocation_monopoly, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spec_alloc_tr_t0, spectrum_allocation_monopoly, theater_ratio, 0, 0.55).
narrative_ontology:measurement(spec_alloc_tr_t15, spectrum_allocation_monopoly, theater_ratio, 15, 0.48).
narrative_ontology:measurement(spec_alloc_tr_t30, spectrum_allocation_monopoly, theater_ratio, 30, 0.45).
narrative_ontology:measurement(spec_alloc_tr_t5, spectrum_allocation_monopoly, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(spec_alloc_be_t0, spectrum_allocation_monopoly, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(spec_alloc_be_t15, spectrum_allocation_monopoly, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(spec_alloc_be_t30, spectrum_allocation_monopoly, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(spec_alloc_be_t5, spectrum_allocation_monopoly, base_extractiveness, 5, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spectrum_allocation_monopoly, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(spectrum_allocation_monopoly, 0.12).
narrative_ontology:affects_constraint(spectrum_allocation_monopoly, cellular_market_concentration).
narrative_ontology:affects_constraint(spectrum_allocation_monopoly, broadband_access_inequality).
narrative_ontology:affects_constraint(spectrum_allocation_monopoly, iot_deployment_fragmentation).

% DUAL FORMULATION NOTE:
% Spectrum allocation monopoly can be decomposed into two structurally distinct constraints: (1) interference_prevention_coordination (ε≈0.08, Mountain/Rope) — the technical requirement to prevent radio interference, a genuine law of physics/engineering, and (2) incumbent_market_protection (ε≈0.68, Snare) — the regulatory choice to limit spectrum access to protect incumbent market position. These constraints are often conflated in policy discourse, with the interference prevention argument used as a cover story for market protection. The current JSON models the combined constraint as experienced by stakeholders. Decomposition would require separate stories for the pure coordination function versus the incumbent protection mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spectrum_allocation_monopoly, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
