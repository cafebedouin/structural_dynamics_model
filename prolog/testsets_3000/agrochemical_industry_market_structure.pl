% ============================================================================
% CONSTRAINT STORY: agrochemical_industry_market_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agrochemical_industry_market_structure, []).

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
 *   constraint_id: agrochemical_industry_market_structure
 *   human_readable: Agrochemical Industry Market Structure and Lock-In
 *   domain: economic/agricultural/environmental
 *
 * SUMMARY:
 *   The agrochemical industry market structure represents a hybrid constraint
 *   that combines genuine coordination functions with systematic extraction.
 *   Over the past 20-30 years, the industry has consolidated from hundreds of
 *   regional suppliers into a handful of global corporations that control
 *   seed genetics, herbicide chemistry, and the intellectual property
 *   frameworks binding them together. This consolidation exhibits all
 *   hallmarks of Tangled Rope: it solves real coordination problems
 *   (standardized seeds enable scalable mechanization and supply chains), but
 *   it also enables asymmetric extraction (smallholder farmers pay input
 *   costs that consume 30-50% of gross revenue, seed patents prevent
 *   cost-saving practices, debt-financing ties farmers into chemical
 *   dependency). The constraint operates differently at different scales:
 *   global consolidation benefits large monoculture operators and enables
 *   technical efficiency; simultaneously, it traps smallholders through
 *   patented seeds, debt financing, and loss of traditional varieties. The
 *   theater ratio (0.45) reflects that while the agrochemical industry
 *   presents itself as essential to food security and agricultural
 *   efficiency, the actual verification of these claims is weak —
 *   productivity gains are often attributed to agrochemicals but may derive
 *   from infrastructure, crop breeding, or environmental luck; safety claims
 *   are supported primarily by industry-funded research; and the lock-in
 *   mechanisms (patents, variety loss, debt) are presented as technical
 *   necessities rather than policy choices.
 *
 * KEY AGENTS:
 *   - Consolidated Agrochemical Manufacturers: Institutional beneficiary (arbitrage exit) — captures rents through IP, pricing power, global logistics standardization; genuinely solves supply chain coordination but captures surplus
 *   - Smallholder Farmers: Primary victim (trapped exit) — bears extraction through input costs, seed patent restrictions, debt cycles, loss of autonomous seed-saving; no meaningful exit options
 *   - Large Monoculture Operators: Powerful beneficiary (mobile exit) — benefits from standardization and high yields but also faces input price volatility and chemical dependency; has diversification options
 *   - National Agricultural Regulators: Institutional actor (constrained exit) — maintains approval processes but increasingly performative; lacks capacity to prevent lock-in dynamics or protect smallholders
 *   - Agroecological Movement Coalition: Organized victims (constrained exit) — building alternative systems but faces scale barriers and market penalties; sees plausible sunset through gradual transition
 *   - Agricultural Biodiversity: Powerless victim (trapped exit) — abstract collective good; narrowed crop genetic diversity due to monoculture incentives; no advocate, no exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agrochemical_industry_market_structure, 0.58).
domain_priors:suppression_score(agrochemical_industry_market_structure, 0.68).
domain_priors:theater_ratio(agrochemical_industry_market_structure, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agrochemical_industry_market_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(agrochemical_industry_market_structure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(agrochemical_industry_market_structure, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agrochemical_industry_market_structure, tangled_rope).
narrative_ontology:human_readable(agrochemical_industry_market_structure, "Agrochemical Industry Market Structure and Lock-In").
narrative_ontology:topic_domain(agrochemical_industry_market_structure, "economic/agricultural/environmental").

domain_priors:requires_active_enforcement(agrochemical_industry_market_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agrochemical_industry_market_structure, consolidated_agrochemical_manufacturers).
narrative_ontology:constraint_beneficiary(agrochemical_industry_market_structure, large_monoculture_operators).
narrative_ontology:constraint_victim(agrochemical_industry_market_structure, smallholder_farmers).
narrative_ontology:constraint_victim(agrochemical_industry_market_structure, agricultural_biodiversity).
narrative_ontology:constraint_victim(agrochemical_industry_market_structure, soil_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by: seed patents preventing saved-seed practices, herbicide-resistant monocultures creating chemical dependency, debt-financing tied to agrochemical purchases, loss of traditional crop varieties. No meaningful exit option. Experiences maximum extraction through input costs, yield stagnation, and debt cycles. Institutional mechanisms (IP law, credit structure, seed variety loss) enforce the trap.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL AGRICULTURAL COOPERATIVE (TANGLED ROPE) — Faces genuine coordination benefits (shared purchasing power, collective storage, market access) but also asymmetric extraction (pricing power of agrochemical firms, volume commitments that lock out competing suppliers, margin compression). Has exit options (organic certification, alternative input sourcing) but faces significant costs (market price penalties, certification delays, replanting timelines). Extraction is real but not total — some agency remains.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSOLIDATED AGROCHEMICAL MANUFACTURER (ROPE) — Experiences the market structure as coordination of a complex value chain: standardized seed varieties enable scalable input production, predictable farmer demand, global logistics integration. Captures rents through intellectual property (seed patents, herbicide tolerant traits) but genuinely solves coordination problems (supply chain predictability, technical standards, information dissemination). Net beneficiary with arbitrage options (can shift to new markets, product lines, regions).
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE-SCALE MONOCULTURE OPERATOR (TANGLED ROPE) — Benefits from agrochemical standardization: predictable input costs at scale, high-yield varieties, pest management consistency. But also experiences extraction: price volatility in input markets, dependency on specific seed-trait combinations, exposure to chemical residue liability, regulatory risk. Has exit options (crop diversification, precision agriculture systems, alternative genetics) but faces capital costs and market uncertainty. Mixed experience: genuine coordination benefits + asymmetric extraction.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: NATIONAL AGRICULTURAL REGULATORY AGENCY (PITON) — Approves seed varieties and pesticides through processes designed to verify safety and efficacy, but the regulatory theater is increasingly performative: approval data generated by industry (structural conflict of interest), regulatory capacity lags chemical innovation, safety thresholds are maintained through political negotiation rather than biological data. Regulation persists due to institutional inertia and baseline legitimacy requirements, but effectiveness in protecting smallholders or biodiversity has declined. Degraded constraint — maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AGROECOLOGICAL MOVEMENT COALITION (SCAFFOLD) — Organized actors (NGOs, research institutions, farmer collectives) building alternative systems: seed saving networks, push-pull pest management, crop diversity, reduced-input systems. Experiences the agrochemical market structure as a temporary lock-in with a plausible sunset. Exit pathways exist (farmer-to-farmer seed networks, reduced chemical intensity, integrated pest management) but face scale barriers and market penalties. Low effective extraction because the coalition has agency and sees an identifiable transition path, even if costly.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing the agrochemical market structure as an inevitable consequence of agricultural efficiency or population feeding requirements: 'high-yield agriculture requires standardized inputs,' 'farmers naturally depend on chemical pest management,' 'market consolidation is inherent to capital-intensive agriculture.' This perspective sees the lock-in as a law of agronomy rather than a contingent institutional arrangement. The structural data contradicts the mountain classification — consolidation, IP enforcement, and seed-saving prohibition are policy choices, not biological necessities. This represents a false summit.
constraint_indexing:constraint_classification(agrochemical_industry_market_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agrochemical_industry_market_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agrochemical_industry_market_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agrochemical_industry_market_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agrochemical_industry_market_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agrochemical_industry_market_structure, TR),
    TR >= 0.70.

:- end_tests(agrochemical_industry_market_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting genuine coordination benefits alongside asymmetric extraction. The agrochemical system solves real problems (supply predictability, pest management, information dissemination) but captures substantial rents through consolidation and IP enforcement. The value increased from 0.38 to 0.58 over 20 years as consolidation deepened and seed patents became globally enforceable. Suppression (0.68): High. Multiple barriers prevent exit: seed patents and IP law enforcement, debt-financing structures tied to agrochemical purchases, loss of traditional crop varieties through seed bank erosion, market penalties for low-input systems, regulatory barriers to alternative inputs. Theater ratio (0.45): Moderate. Agrochemical firms present their products as technically necessary and scientifically validated, but verification is limited: industry funds most safety research, productivity claims conflate multiple factors, lock-in mechanisms are presented as inevitable rather than policy-contingent. The theater is maintained but not dominant — the coordination function is real enough that performative elements remain secondary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent perceptions of whether the agrochemical market structure is 'natural' or 'contingent.' The manufacturer and large operator see it as efficient coordination. The smallholder sees it as a trap. The coalition sees it as a temporary lock-in being dismantled. The regulator sees it as performative justification. The analyst risks seeing it as inevitable agricultural requirement. The perspectival gap reveals that this is fundamentally a Tangled Rope — the gap itself is the signature of mixed coordination and extraction. If all perspectives converged on 'this is pure coordination' it would be Rope; if all converged on 'this is pure extraction' it would be Snare. The divergence proves the hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) values differentiate across perspectives through their exit options and beneficiary/victim status. Smallholders with trapped exit and victim status experience maximum d (high extraction experienced). Large operators with mobile exit and partial beneficiary status experience lower d (mixed experience). The consolidated manufacturer with arbitrage exit and primary beneficiary status experiences minimum d (extraction flows toward them). Regulatory agencies with constrained exit but neither clear beneficiary nor victim status occupy middle positions. The agroecological coalition with constrained exit but active agency experiences moderate d despite victim status because their exit pathways are real, even if costly. The directionality computation reflects that structural position determines experienced extraction more than institutional power alone.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The agrochemical constraint resolves mandatrophy by showing that consolidation creates both coordination (genuine efficiency in supply chains, logistics, R&D scale) and extraction (rent capture through IP, pricing power, lock-in mechanisms). Neither single type is correct. The manufacturer's rope is their genuine structural experience (they ARE coordinating a complex system). The smallholder's snare is their genuine structural reality (they ARE trapped with no exit). The scaffold's temporary lock-in is a genuine structural observation (alternatives exist but face barriers). The regulatory piton is a genuine observation (approval processes have become hollow). The analytical mountain is a false summit (consolidation is policy-contingent, not law-necessary). The correct classification is TANGLED ROPE because the constraint simultaneously: (1) solves real coordination problems (supply chain standardization enabling scale), (2) exhibits asymmetric extraction (rents concentrated through consolidation and IP), (3) requires active enforcement (patent law, regulatory approval mechanisms, credit structures), (4) has beneficiaries (manufacturers, large operators) and victims (smallholders, biodiversity, soil health). If any of these four elements were absent, the classification would shift. The presence of all four makes Tangled Rope the structurally correct type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seed_patent_enforcement_mechanism,
    'Are seed patents enforced primarily through legal/technical mechanisms (Terminator genes, sterility, DRM) or through regulatory frameworks and farmer compliance norms?',
    'Tracking of enforcement actions (litigation, prosecution), analysis of farmer awareness and perceived legality, comparison of technical vs legal enforcement burden, study of seed-saving rates in regions with/without patent enforcement infrastructure',
    'If primarily legal/regulatory: the constraint requires active enforcement infrastructure (Tangled Rope). If primarily technical: the constraint emerges semi-naturally (appears more like mountain to farmers but is actually Snare). If primarily norm-based: peasant compliance may be vulnerable to reframing (scaffold perspective strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seed_patent_enforcement_mechanism, empirical, 'Primary mechanism for seed patent enforcement').

omega_variable(
    alternative_input_viability,
    'Can agronomically viable and economically competitive alternatives to synthetic agrochemicals be deployed at scale in diverse agroecological contexts?',
    'Multi-year comparative trials across agroecologies; cost accounting including externalities and long-term soil health; farmer-adoption rates when alternatives have price parity or subsidy support; market development timeline for scaled alternative input systems',
    'If viable: scaffold sunset is real (10-20 year transition feasible). If not viable: the agrochemical lock-in is partly structural (coordination genuinely requires high-yield inputs) and partly extractive (firms maintain high prices despite viability). Classification would shift toward Rope from some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_input_viability, empirical, 'Viability of scaled alternatives to synthetic agrochemicals').

omega_variable(
    consolidation_efficiency_claim,
    'Does agrochemical industry consolidation produce measurable efficiency gains (R&D productivity, input cost reduction, productivity increase) relative to distributed/competitive markets, or does consolidation primarily capture rent?',
    'Comparative analysis of R&D output per dollar before and after major consolidations (Syngenta-ChemChina 2016, Bayer-Monsanto 2018, Dow-DuPont 2017); input price trends relative to commodity prices; farmer profit margin trends; productivity gains vs alternative explanations (climate luck, infrastructure investment)',
    'If consolidation produces efficiency: the constraint is primarily coordination (Rope from multiple perspectives). If efficiency is not demonstrated: the constraint is primarily extraction masked as coordination (Snare or Tangled Rope). This determines whether the current market structure is structurally necessary or contingently maintained through rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_efficiency_claim, empirical, 'Whether consolidation produces genuine efficiency gains').

omega_variable(
    smallholder_farmer_coalition_power,
    'If smallholder farmers organize collectively (through cooperatives, supply contracts, bargaining associations), can they achieve sufficient countervailing power to reduce extraction?',
    'Analysis of collective bargaining outcomes in organized regions (Fair Trade, Rainforest Alliance, farmer unions); pricing power achieved by organized buyer groups; comparative extraction rates between organized and isolated farmers; sustainability of coalition structures under industry pressure',
    'If coalition power is achievable: powerless classification may shift to organized (reduces experienced chi). If coalitions are systematically undermined: powerless is reinforced, and snare becomes more entrenched. This determines whether the constraint is structural or contingent on coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smallholder_farmer_coalition_power, empirical, 'Effectiveness of collective farmer organization to reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agrochemical_industry_market_structure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agrochem_tr_t0, agrochemical_industry_market_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(agrochem_tr_t10, agrochemical_industry_market_structure, theater_ratio, 10, 0.4).
narrative_ontology:measurement(agrochem_tr_t20, agrochemical_industry_market_structure, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(agrochem_be_t0, agrochemical_industry_market_structure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(agrochem_be_t10, agrochemical_industry_market_structure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(agrochem_be_t20, agrochemical_industry_market_structure, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agrochemical_industry_market_structure, resource_allocation).
narrative_ontology:affects_constraint(agrochemical_industry_market_structure, seed_patenting_regime).
narrative_ontology:affects_constraint(agrochemical_industry_market_structure, agricultural_credit_structures).
narrative_ontology:affects_constraint(agrochemical_industry_market_structure, pesticide_resistance_evolution).

% DUAL FORMULATION NOTE:
% The agrochemical market structure is downstream of the seed patenting regime and agricultural credit structures, which create the enforcement mechanisms for lock-in. It affects pesticide resistance evolution, which creates new suppression mechanisms as spray failures drive higher application rates. These three constraints form an institutional family: seed patents + credit structures → market structure consolidation → pesticide resistance → higher extraction pressure. Each has its own ε value reflecting its specific structural role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agrochemical_industry_market_structure, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
