% ============================================================================
% CONSTRAINT STORY: vaccine_distribution_equity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_distribution_equity, []).

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
 *   constraint_id: vaccine_distribution_equity
 *   human_readable: Vaccine Distribution Equity Constraint
 *   domain: public_health/global_governance
 *
 * SUMMARY:
 *   Vaccine distribution equity during pandemic response creates a structural
 *   constraint where multiple conflicting coordination logics collide.
 *   High-income countries, pharmaceutical manufacturers, and multilateral
 *   organizations all experience coordination benefits (rope perspective) —
 *   the distribution mechanism enables predictable supply and market access.
 *   Simultaneously, low-income countries and vaccine-insecure populations
 *   experience pure extraction (snare perspective) — they face suppression
 *   via resource scarcity, pricing power, and donor-country conditionality.
 *   The constraint exhibits both coordination function (genuinely enabling
 *   large-scale immunization) and asymmetric extraction (systematically
 *   advantaging wealthy actors). This makes it a canonical tangled_rope:
 *   cannot be classified as pure coordination, cannot be classified as pure
 *   extraction, but must be classified as both simultaneously. The
 *   distribution system is enforced through intellectual property law,
 *   contract enforcement, and multilateral financing agreements. Theater
 *   increases over time as equity rhetoric intensifies while actual inequity
 *   persists (COVAX targets announced but missed, tiered pricing creates
 *   appearance of access while maintaining profitability, political
 *   commitment to equity without structural change). The constraint's
 *   temporal trajectory shows extractiveness rising early (when capacity is
 *   most constrained and competition fiercest) and stabilizing as production
 *   scales, but theater continuing to rise as gap between equity rhetoric and
 *   actual outcomes widens.
 *
 * KEY AGENTS:
 *   - Vaccine-insecure populations in low-income countries: Primary victims (powerless/trapped) — structurally dependent on external allocation decisions with no bargaining capacity or exit options
 *   - Low-income country governments: Secondary victims (moderate/constrained) — organized state actors but resource-limited and competing in global market where they hold weak position
 *   - High-income country governments: Primary beneficiaries (institutional/arbitrage) — can prioritize domestic populations, negotiate manufacturer contracts, secure supply at favorable terms
 *   - Pharmaceutical manufacturers: Primary beneficiaries (institutional/arbitrage) — control production, set prices within constraints, choose which markets to prioritize
 *   - Multilateral organizations (WHO, GAVI, COVAX): Organized coordinators (organized/constrained) — attempt equity through negotiation but constrained by manufacturer profit requirements and state sovereignty
 *   - Patent and IP enforcement system: Institutional actor (institutional/arbitrage) — maintains monopoly pricing and technology control through legal mechanism
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-contingent scarcity as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_distribution_equity, 0.58).
domain_priors:suppression_score(vaccine_distribution_equity, 0.62).
domain_priors:theater_ratio(vaccine_distribution_equity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_distribution_equity, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_distribution_equity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vaccine_distribution_equity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_distribution_equity, tangled_rope).
narrative_ontology:human_readable(vaccine_distribution_equity, "Vaccine Distribution Equity Constraint").
narrative_ontology:topic_domain(vaccine_distribution_equity, "public_health/global_governance").

domain_priors:requires_active_enforcement(vaccine_distribution_equity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_distribution_equity, high_income_countries).
narrative_ontology:constraint_beneficiary(vaccine_distribution_equity, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(vaccine_distribution_equity, low_income_countries).
narrative_ontology:constraint_victim(vaccine_distribution_equity, vaccine_insecure_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINE-INSECURE POPULATIONS (SNARE) — Trapped without exit options. Lack economic resources to purchase vaccines at market rates, lack political voice in procurement negotiations, and depend entirely on aid allocation decisions made by higher-power actors. Face maximum suppression: no alternatives, no bargaining capacity, no exit. Classification as snare reflects the absence of coordination benefit — the distribution constraint imposes pure extraction.
constraint_indexing:constraint_classification(vaccine_distribution_equity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME COUNTRY GOVERNMENT (TANGLED ROPE) — Constrained by budget limits and dependence on donor coordination. Experiences genuine coordination function (COVAX mechanism, WHO guidance enable collective action at scale). But also experiences asymmetric extraction: resources flow away during critical window, prices are set externally, and domestic capacity building is deprioritized. High suppression via resource scarcity and structural inequality. Moderate power via organized state apparatus but constrained by global competition.
constraint_indexing:constraint_classification(vaccine_distribution_equity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Primary beneficiary with arbitrage capacity. Can choose which markets to supply, which prices to charge, which production facilities to prioritize. Experiences distribution constraint as pure coordination mechanism: supply agreements with high-income countries enable predictable revenue and market access. No suppression from this actor's perspective — they have exit options and direct benefit flow. Classification as rope reflects net positive extraction benefit.
constraint_indexing:constraint_classification(vaccine_distribution_equity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-INCOME COUNTRY GOVERNMENT (ROPE) — Primary beneficiary with strongest arbitrage capacity. Can purchase at volume, negotiate priority supply windows, secure manufacturing contracts with conditional IP terms. Experiences distribution constraint as coordination mechanism enabling large-scale vaccine access with domestic political benefit. Asymmetry runs in their favor — they extract value from lower-income actors who bid against them for limited supply. Low suppression from this perspective.
constraint_indexing:constraint_classification(vaccine_distribution_equity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTILATERAL HEALTH ORGANIZATION (TANGLED ROPE) — Organized actors (WHO, GAVI, COVAX) attempt to coordinate equity through negotiation and financing mechanisms. Experience genuine coordination function: pooled procurement reduces per-unit costs, tiered pricing enables access across income levels, information sharing improves allocation efficiency. But also experience extraction: constrained by state sovereignty (cannot mandate supply), constrained by manufacturer profit requirements (tiered pricing erodes revenue targets), constrained by high-income country political pressure. High enforcement requirements sustain the mechanism. Moderate effective extraction via these binding constraints.
constraint_indexing:constraint_classification(vaccine_distribution_equity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PATENT ENFORCEMENT SYSTEM (PITON) — Intellectual property protections for vaccine technology are maintained through institutional inertia and legal enforcement, but their functional role in incentivizing innovation has degraded once the technology is proven and mass-production is the bottleneck. Theater ratio high: patents appear to protect innovation but actually protect monopoly pricing once demand is known and public funding has de-risked development. The patent framework persists because it benefits high-income countries and manufacturers, not because it solves a real coordination problem at scale-production stage. Classification driven by high theater ratio despite moderate extracted value.
constraint_indexing:constraint_classification(vaccine_distribution_equity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, vaccine supply is fundamentally scarce during rapid-response scenarios: manufacturing capacity takes months to scale, global coordination is inherently difficult, and some prioritization of demand is mathematically necessary. This perspective naturalizes the distribution constraint as immutable — scarcity of manufacturing capacity is a fact of physics/economics. However, the structural data contradicts this mountain classification: actual supply constraints are far less severe than political/pricing constraints. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(vaccine_distribution_equity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_distribution_equity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_distribution_equity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_distribution_equity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_distribution_equity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_distribution_equity, TR),
    TR >= 0.70.

:- end_tests(vaccine_distribution_equity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts significant value from low-income actors to high-income actors and manufacturers through multiple mechanisms: pricing power (doses cost more in competitive markets), prioritization (high-income countries secure earlier shipments), and IP barriers (generic manufacturing limited). But extraction is not maximal (0.70+) because: (1) some vaccination does occur in low-income countries via aid and COVAX, (2) multilateral mechanisms provide some negotiating power, (3) public funding was substantial (reducing extraction relative to pure-market scenario). Suppression (0.62): High. Low-income countries face multiple binding constraints: budget limits prevent market purchase, political dependence on donor countries, limited manufacturing capacity, and lack of IP/contract negotiating power. Suppression is structural (resource scarcity) and policy-driven (pricing, IP enforcement, donor conditionality). Theater ratio (0.55): Moderate-high and increasing over interval. Early phase: genuine coordination problem (scarce doses need allocation). Later phase: manufacturing scales but political/pricing constraints persist while equity rhetoric intensifies. Theater rises as COVAX targets are missed, tiered pricing is praised while profitability remains, and political commitment statements are unmatched by structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival disagreement on whether the core problem is coordination or extraction. High-income countries and manufacturers perceive genuine coordination benefit — the distribution system solves the real problem of allocating scarce goods at continental/global scale. Multilateral organizations perceive mixed coordination and extraction — they genuinely solve some coordination problems through pooling and negotiation, but are constrained by the asymmetries they cannot overcome. Low-income countries perceive extraction with minimal coordination benefit — for them, the system's primary function is to preserve pricing power and manufacturing control, not to solve a collective action problem. Vaccine-insecure populations perceive pure extraction — they are systematically excluded from access via pricing and have no bargaining role. The mountain perspective (scarcity is immutable) conflicts sharply with snare perspective (distribution choices create artificial scarcity). This gap reveals the analytical challenge: in early pandemic phase (genuine capacity constraints), mountain/rope perspectives are empirically supported. But as capacity scales, the distribution problem becomes institutional/political, not physical, and snare/tangled_rope perspectives become empirically supported. The constraint's type should shift over the interval, but base_properties must be static, so they reflect the time-averaged classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position: beneficiary status, victim status, and exit options. High-income countries and manufacturers are beneficiaries with strong exit options (can walk away from equity commitments, can choose profitable markets) — they experience low or negative effective extraction. Low-income countries are victims with constrained exit (must participate in distribution system to access vaccines at all) — they experience high effective extraction. Multilateral organizations are beneficiaries of coordination (they reduce transaction costs for both sides) but constrained victims of the asymmetries they cannot overcome (they cannot mandate fair pricing or supply priority). The piton perspective derives from institutional actors (patent system) who maintain enforcement of a mechanism (IP protection) that is dysfunctional at the distribution scale — it has become primarily extractive theater. The mountain perspective risks misclassifying institutional/policy choices as natural law by conflating 'allocation is necessary' (true) with 'this allocation mechanism is necessary' (false).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR TANGLED_ROPE: This constraint resolves the mandatrophy by demonstrating that coordination and extraction are not mutually exclusive. The distribution system genuinely solves a real coordination problem (allocating scarce manufacturing output at global scale) while simultaneously extracting asymmetrically (systematically advantaging wealthy actors). Both functions are real. The mandatrophy is resolved by recognizing that the claimed type (tangled_rope) is the correct unified classification, not by trying to choose between rope and snare. The perspectival disagreement is real and reflects genuine structural differences (different agents have different actual experiences of the constraint), but the base classification is unified: this is a hybrid mechanism that coordinates while extracting. The false summit detection would flag the mountain perspective: scarcity at the manufacturing stage is real, but the distribution mechanism's primary effect at scale is institutional/extractive, not physical. Theater increase over time (equity rhetoric without structural change) is a mandatrophy warning sign: the coordination benefit initially justifies the extraction, but as theater increases, the extraction persists without corresponding coordination payoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manufacturing_scarcity_vs_pricing_scarcity,
    'Is the primary constraint physical manufacturing capacity or policy/pricing decisions that reduce effective supply?',
    'Comparison of actual production capacity utilization vs maximum feasible production; analysis of counterfactual scenarios (if prices were capped at cost + 20%, how much total supply would have been produced?); post-constraint data on manufacturing expansion rates',
    'If manufacturing-limited: constraint is closer to mountain (objective scarcity). If pricing-limited: constraint is snare (policy choice extracting from vulnerable populations). This determines whether the mountain perspective is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_scarcity_vs_pricing_scarcity, empirical, 'Whether supply scarcity is driven by manufacturing capacity or pricing/policy').

omega_variable(
    ip_patent_necessity_for_innovation,
    'Did patent protections on vaccine technology materially accelerate development, or was acceleration driven primarily by public funding and competitive prestige?',
    'Historical analysis of R&D investment timelines, funder categories (public vs private), and stated developer motivations; comparison with non-patented vaccine development timelines in prior pandemics; analysis of licensing practices post-development',
    'If patents were necessary: piton classification is misapplied (the system has real function). If unnecessary: piton classification confirmed (patents persist through inertia/law enforcement, not functional coordination). This affects whether IP enforcement is treated as degraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ip_patent_necessity_for_innovation, empirical, 'Whether patent protection was functionally necessary for vaccine development').

omega_variable(
    covax_coordination_effectiveness,
    'Did COVAX/multilateral mechanisms genuinely reduce inequity, or primarily create the appearance of equity work while preserving high-income advantage?',
    'Quantitative comparison: actual vaccination rates achieved in low-income countries under COVAX vs counterfactual of purely market-driven distribution; post-hoc analysis of which countries received doses and timeline adherence; analysis of financing flows and actual subsidy amounts',
    'If effective coordination: COVAX rope/tangled_rope classification confirmed. If primarily theater: COVAX should be reclassified as piton (performative mechanism preserving inequality). This affects whether organized actors are genuine coordinators or extractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covax_coordination_effectiveness, empirical, 'Whether multilateral coordination mechanisms reduced actual inequity').

omega_variable(
    suppression_mechanism_source,
    'Is suppression in low-income countries primarily structural (resource scarcity, capacity limits) or policy-driven (pricing, IP enforcement, donor conditions)?',
    'Decomposition analysis: isolate infrastructure/capacity constraints vs policy/pricing constraints; counterfactual modeling of alternative institutional arrangements; post-constraint institutional reforms and measured impact',
    'If structural: suppression is inherent, closer to mountain constraint. If policy-driven: suppression is extractive, validates snare classification for trapped victims. This affects mitigation strategy selection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression is structural scarcity or policy-driven inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_distribution_equity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vaxdist_tr_t0, vaccine_distribution_equity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vaxdist_tr_t6, vaccine_distribution_equity, theater_ratio, 6, 0.48).
narrative_ontology:measurement(vaxdist_tr_t12, vaccine_distribution_equity, theater_ratio, 12, 0.55).
narrative_ontology:measurement(vaxdist_tr_t24, vaccine_distribution_equity, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(vaxdist_be_t0, vaccine_distribution_equity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vaxdist_be_t6, vaccine_distribution_equity, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(vaxdist_be_t12, vaccine_distribution_equity, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(vaxdist_be_t24, vaccine_distribution_equity, base_extractiveness, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_distribution_equity, resource_allocation).
narrative_ontology:boltzmann_floor_override(vaccine_distribution_equity, 0.18).
narrative_ontology:affects_constraint(vaccine_distribution_equity, pandemic_intellectual_property_barriers).
narrative_ontology:affects_constraint(vaccine_distribution_equity, global_supply_chain_concentration).
narrative_ontology:affects_constraint(vaccine_distribution_equity, health_financing_inequity).

% DUAL FORMULATION NOTE:
% Vaccine distribution equity decomposes into at least three structurally distinct constraints: (1) manufacturing capacity scarcity (ε≈0.15, closer to mountain), (2) IP/patent enforcement restricting generic production (ε≈0.45, closer to snare), (3) financing inequality limiting purchase power (ε≈0.60, snare). This story treats the integrated distribution constraint at system level. Downstream constraints specify the mechanical components of the extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_distribution_equity, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
