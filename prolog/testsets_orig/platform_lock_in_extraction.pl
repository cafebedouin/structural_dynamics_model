% ============================================================================
% CONSTRAINT STORY: platform_lock_in_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_lock_in_extraction, []).

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
 *   constraint_id: platform_lock_in_extraction
 *   human_readable: Platform Lock-In Extraction
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Platform lock-in extraction represents a structural asymmetry where
 *   digital intermediaries capture value through network effects, data
 *   aggregation, and algorithmic opacity while making exit prohibitively
 *   costly for dependent merchants and constrained consumers. The constraint
 *   exhibits the full diagnostic spectrum: pure coordination from the
 *   platform operator's view, pure extraction from trapped merchants' view,
 *   mixed coordination-extraction from mid-market sellers, regulatory theater
 *   from organized coalitions attempting intervention, and temporary
 *   scaffolding from emerging decentralized alternatives. The rising
 *   extractiveness (0.42→0.68) over the interval reflects commission
 *   increases, algorithmic deranking of independent seller channels, and
 *   deepening ecosystem lock-in as consumer data and transaction history
 *   accumulate. Theater_ratio increases from 0.35 to 0.55 as regulatory
 *   interventions (interoperability mandates, transparency requirements)
 *   mount without breaking the underlying lock-in mechanism—the performative
 *   appearance of constraint without structural change.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures value through network effects, data aggregation, and commission structure; experiences constraint as coordination mechanism
 *   - Dependent Merchants: Primary victim (powerless/trapped) — structurally locked in through customer relationship lock-in, reputation capital, algorithmic visibility dependencies; no viable exit
 *   - Price-Constrained Consumers: Secondary victim (powerless/trapped) — trapped through ecosystem integration, data history, switching costs, and algorithmic price discovery opacity
 *   - Mid-Market Sellers: Secondary victim (moderate/constrained) — experience both coordination benefits and extraction; high exit costs but surmountable through diversification
 *   - Seller Coalition: Organized victim (organized/constrained) — collective bargaining power raises negotiation capacity but does not eliminate structural lock-in; benefits coordination but constrained by platform policy changes
 *   - Regulatory Coalition: Organized actor attempting intervention (organized/mobile) — sees lock-in as solvable through mandates but lacks enforcement capacity; regulatory activity becomes theatrical
 *   - Open-Platform Initiative: Alternative provider (organized/mobile) — building decentralized alternatives with genuine sunset logic; represents structural pathway to reduced extraction if standards mature
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (data lock-in, algorithmic opacity) as inevitable properties of digital markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_lock_in_extraction, 0.68).
domain_priors:suppression_score(platform_lock_in_extraction, 0.72).
domain_priors:theater_ratio(platform_lock_in_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_lock_in_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(platform_lock_in_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(platform_lock_in_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_lock_in_extraction, snare).
narrative_ontology:human_readable(platform_lock_in_extraction, "Platform Lock-In Extraction").
narrative_ontology:topic_domain(platform_lock_in_extraction, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(platform_lock_in_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_lock_in_extraction, platform_operator).
narrative_ontology:constraint_victim(platform_lock_in_extraction, merchant_sellers).
narrative_ontology:constraint_victim(platform_lock_in_extraction, creator_economy_workers).
narrative_ontology:constraint_victim(platform_lock_in_extraction, consumer_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MERCHANT (SNARE) — Small merchants and creators face inescapable platform dependence. Switching platforms requires rebuilding customer relationships, algorithmic discovery from zero, and losing accumulated reputation capital. Economic dependency on platform revenue with no viable alternative channels. Network effects mean exit is structurally impossible for most agents.
constraint_indexing:constraint_classification(platform_lock_in_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRICE-CONSTRAINED CONSUMER (SNARE) — Consumers trapped in platform ecosystems through data lock-in, switching costs, and ecosystem integration (payment systems, recommendations, review histories). Price discovery becomes impossible when competing platforms obscure pricing through algorithmic curation and personalized pricing. Limited transparent alternatives.
constraint_indexing:constraint_classification(platform_lock_in_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-MARKET SELLER (TANGLED ROPE) — Medium-sized merchants experience genuine coordination benefits (access to logistics, payment infrastructure, customer reach) alongside asymmetric extraction (commission increases, algorithm opacity, policy changes). Costs of exit are high but surmountable; some negotiating power exists. Mixed coordination and extraction.
constraint_indexing:constraint_classification(platform_lock_in_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: solving the matching problem between buyers and sellers, enabling transactions at scale. Network effects benefit the operator directly through increased transactional volume and data aggregation. Sees lock-in as solving a fundamental coordination problem.
constraint_indexing:constraint_classification(platform_lock_in_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (PITON) — Regulators and antitrust authorities see lock-in as a problem requiring intervention, but enforcement mechanisms are largely theatrical: consent decrees, transparency mandates, interoperability rules often fail to break network effects. Theater ratio high — regulatory activity appears to constrain extraction but structural lock-in persists. Sunset logic: if interoperability standards mature (DMA provisions, potential data portability), extraction mechanism degrades.
constraint_indexing:constraint_classification(platform_lock_in_extraction, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-PLATFORM INITIATIVE (SCAFFOLD) — Decentralized platforms, open-source marketplaces, and federated commerce protocols represent temporary coordination scaffolding designed to reduce extraction by breaking network effect locks. Low extraction because the initiative has agency and a clear sunset: if portability standards and multi-platform aggregation mature, lock-in extraction mechanism becomes structurally unviable.
constraint_indexing:constraint_classification(platform_lock_in_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: SELLER COALITION (TANGLED ROPE) — Organized seller groups (unions, associations) benefit from platform coordination (access to logistics, payment systems, customer reach at scale) while experiencing asymmetric extraction (commission rates, algorithm opacity, policy changes without consultation). Coalition increases exit costs through collective bargaining but does not eliminate structural lock-in.
constraint_indexing:constraint_classification(platform_lock_in_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Viewed from civilizational/universal scale, network effects and data network effects appear immutable: platforms require critical mass to function, and switching costs are inherent to network coordination. Lock-in might appear as inevitable property of digital markets. However, this perspective naturalizes a contingent institutional arrangement: switching costs and data lock-in are policy choices (data portability, interoperability standards, cross-platform aggregation), not physical laws. Engine flags as false summit.
constraint_indexing:constraint_classification(platform_lock_in_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_lock_in_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_lock_in_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_lock_in_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_lock_in_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_lock_in_extraction, TR),
    TR >= 0.70.

:- end_tests(platform_lock_in_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, with rising trend. Platform commissions (15-30% on marketplace transactions) exceed documented service costs (3-8% for payment processing, fraud detection, hosting). Additional extraction occurs through algorithm-driven seller deranking (forced advertising purchases to restore visibility), data lock-in (merchant sales history, customer relationships trapped on platform), and switching cost accumulation. The 0.42→0.68 trajectory reflects systematic commission increases and algorithmic opacity deepening. Mid-interval value 0.58 captures 5-year period when major platforms increased commission rates 2-3x on smaller sellers. Suppression (0.72): High and structural. Merchants cannot exit without abandoning established customer bases and algorithmic visibility—true exit cost is reputational/relationship capital loss, not just financial transaction cost. Consumers face payment system lock-in, personalized pricing algorithms that obscure true price discovery, and recommendation algorithm opacity. Platform policies forbid or make costly any merchant activity that directs customers off-platform (affiliate link suppression, contact extraction prohibition). Network effects and data lock-in function as enforcement mechanisms—the platform does not need explicit coercion because structural dependencies do the work. Theater ratio (0.55): Moderate and rising. Regulatory mandates (DMA interoperability rules, platform transparency requirements, seller-centered policy debates) create theatrical activity without structural change—lock-in persists through data aggregation and network effects despite regulatory pressure. Theater has increased from 0.35 to 0.55 as regulatory activity mounts without solving underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Platform operator sees Rope (pure coordination problem: matching buyers and sellers). Trapped merchants see Snare (pure extraction with no escape). Mid-market sellers see Tangled Rope (genuine coordination benefits alongside asymmetric extraction). Regulatory coalition sees Piton (their interventions appear to constrain extraction but theater dominates; actual lock-in persists). Open-platform initiative sees Scaffold (alternative pathways with sunset logic). The gap reflects genuine structural divergence, not mere differences in interpretation. Each perspective's classification is correct from its context. The platform operator's Rope is their actual structural experience (coordination problem solving = their activity). The trapped merchant's Snare is their actual structural experience (locked in through relationships and visibility). Neither is a misunderstanding—they have genuinely different structural relationships to the same constraint. The perspectival gap is the constraint itself: one agent's coordination benefit is another agent's extraction trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: platform_operator (institutional/arbitrage). Extracts through commission structure, algorithmic visibility control, and data aggregation. Direction of extraction flow: merchants and consumers → platform operator. Victims: merchant_sellers (powerless/trapped, moderate/constrained), consumer_price_discovery (powerless/trapped), seller organizations (organized/constrained). Each victim group has different exit capacity and different experienced extraction severity. The asymmetry is the core snare structure: platform creates value coordination (solves matching problem) AND extracts more value than service justifies (commission premium over competitive baseline). Snare gate requires victims + beneficiaries + high suppression + high extraction. All four criteria met. Chi calculation: Platform operator d ≈ 0.05 → f(d) ≈ -0.12 → χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (experiences negative extraction = net benefit). Trapped merchant d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.97 (experiences maximum extraction). Mid-market seller d ≈ 0.65 → f(d) ≈ 1.00 → χ ≈ 0.68 × 1.00 × 1.0 ≈ 0.68 (experiences moderate extraction). Scope modifier σ(global) = 1.2 applies to all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   Platform lock-in resolves mandatrophy by distinguishing genuine coordination (Rope) from extraction using coordination as cover story (Snare + theater). The platform operator truthfully experiences this as solving a coordination problem—they are. But the asymmetry (merchant extraction plus consumer lock-in) reveals that solving the matching problem is not the constraint's primary function. The primary function is extraction through lock-in. Theater_ratio (0.55) is moderate, not high—this is not primarily theatrical. The constraint is structurally real extraction, not performative. Snare classification confirmed. The scaffold and piton perspectives represent intervention attempts (regulatory theater, open-platform sunset logic) that acknowledge extraction exists. The false mountain perspective (naturalizing lock-in as inevitable) is correctly flagged as false summit by the engine—interoperability standards, data portability, and cross-platform aggregation are policy choices, not physical laws. Mandatrophy is resolved through the perspectival presheaf: the constraint is genuinely Rope from operator perspective, genuinely Snare from merchant perspective, and the divergence itself is the diagnostic signal that justifies intervention—when one agent's coordination is another agent's extraction, the asymmetry reveals extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_measurement,
    'What constitutes the true exit cost for platform-dependent agents: financial transaction costs, reputation capital loss, or identity lock-in through social graph integration?',
    'Empirical tracking of merchant margins post-exit vs margins on platform; consumer switching behavior when price differentials exceed switching costs; measurement of reputation loss when accounts migrate',
    'If primarily financial: exit_options downgrades from trapped to constrained for some agents, reducing effective extraction. If primarily reputational/identity-based: identity_locked becomes more accurate classification than trapped for subset of agents, changing perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Measurement of true exit costs: financial, reputational, or identity-based').

omega_variable(
    network_effect_inevitability,
    'Are network effects in platform markets a structural inevitability or a policy-contingent outcome of data lock-in and algorithmic opacity?',
    'Comparative analysis of interoperability standards in payment systems (visa/mastercard coexistence, open banking regulations) vs proprietary platforms; measurement of switching rates when portability is available',
    'If inevitable: mountain classification gains credibility and lock-in becomes accepted cost of coordination. If policy-contingent: opens pathway for regulatory intervention; lock-in is snare masquerading as coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, conceptual, 'Whether network effects are inevitable or policy-contingent').

omega_variable(
    commission_extraction_vs_service_cost,
    'What portion of platform commissions represents genuine service coordination (logistics, payments, dispute resolution) versus pure extraction rent?',
    'Cost accounting of actual services provided; comparison with transparent-cost competitor platforms; analysis of margin compression in competitive vs monopolistic platform segments',
    'If service cost dominates: extractiveness should be downgraded, constraining classification toward Tangled Rope. If extraction dominates: snare classification reinforced, theater_ratio increases as service justification becomes cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commission_extraction_vs_service_cost, empirical, 'Decomposition of commission rates into service cost vs extraction rent').

omega_variable(
    algorithmic_opacity_intentionality,
    'Is algorithmic opacity (seller deranking, recommendation suppression, search visibility manipulation) an incidental byproduct of complex systems or an intentional suppression mechanism to increase merchant dependence?',
    'Forensic analysis of algorithm design choices; comparison with transparent-algorithm platforms; whistleblower/internal documentation on recommendation system intent',
    'If incidental: suppression metric should be lowered; treats extraction as byproduct rather than design feature. If intentional: suppression is confirmed as core extraction mechanism; snare classification strengthened; moves from ''trap caused by network effects'' to ''trap designed into algorithms''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_opacity_intentionality, empirical, 'Whether algorithmic opacity is incidental or intentional suppression').

omega_variable(
    interoperability_viability,
    'Can open standards and data portability actually break platform lock-in at scale, or do they create coordination failures that reduce service quality below pre-interop baseline?',
    'Pilot programs with federated commerce protocols, cross-platform aggregation systems; measurement of merchant satisfaction and consumer experience; analysis of whether fragmented platforms lose critical services (fraud detection, recommendation quality, payment security)',
    'If viable: scaffold and open-platform perspectives confirmed; extractiveness can decline through regulatory sunset. If viability fails: lock-in becomes ''least-bad'' solution; reclassify toward rope (coordination benefit) from moderate/organized perspectives; constrains intervention pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_viability, empirical, 'Whether open standards can break lock-in without degrading service quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_lock_in_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platform_lock_in_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(plat_tr_t5, platform_lock_in_extraction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(plat_tr_t10, platform_lock_in_extraction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platform_lock_in_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(plat_be_t5, platform_lock_in_extraction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(plat_be_t10, platform_lock_in_extraction, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_lock_in_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_lock_in_extraction, 0.12).
narrative_ontology:affects_constraint(platform_lock_in_extraction, data_portability_regulation).
narrative_ontology:affects_constraint(platform_lock_in_extraction, algorithmic_transparency_mandate).
narrative_ontology:affects_constraint(platform_lock_in_extraction, digital_market_concentration).

% DUAL FORMULATION NOTE:
% Platform lock-in extraction is downstream of digital network effects and data aggregation economics. Upstream constraints (network_effects_inevitability, data_ownership_asymmetry) establish preconditions; this constraint formalizes the extraction mechanism that locks in dependent agents. Separate stories decompose regulatory intervention (interoperability_mandate_effectiveness, platform_transparency_theater) from the structural lock-in itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_lock_in_extraction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
