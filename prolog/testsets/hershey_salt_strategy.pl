% ============================================================================
% CONSTRAINT STORY: hershey_salt_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hershey_salt_strategy, []).

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
 *   constraint_id: hershey_salt_strategy
 *   human_readable: Hershey's Salt Inclusion Strategy
 *   domain: economic/food_manufacturing
 *
 * SUMMARY:
 *   Hershey's systematic inclusion of salt across its product line creates a
 *   structural extraction mechanism targeting consumers unable to exit the
 *   constraint. The strategy increases demand through hedonic manipulation
 *   while obscuring the cumulative sodium burden to health-sensitive
 *   populations. From the beneficiary's perspective (Hershey and
 *   shareholders), the constraint functions as coordination — salt reduces
 *   manufacturing costs, extends shelf life, and strengthens brand lock-in
 *   through habit formation. From the victim's perspective (sodium-sensitive
 *   consumers), the constraint functions as a snare — health conditions make
 *   exit costly or impossible, and marketing framing ('enhanced flavor,'
 *   'indulgence') obscures the sodium accumulation. The constraint exhibits
 *   all six types depending on observer position: coordination mechanism
 *   (beneficiary), temporary regulatory failure with sunset (public health
 *   advocates), degraded industry standard maintained by competitive inertia
 *   (piton), immutable human biology (false mountain), mixed coordination and
 *   extraction (health-conscious consumers), and pure extraction (powerless
 *   populations). The theater ratio shows a gradual increase as sodium
 *   content becomes normalized and the framing shifts from 'salty treat' to
 *   'premium flavor enhancement' — the performative component of the strategy
 *   grows over time.
 *
 * KEY AGENTS:
 *   - Hershey Corporation & Shareholders: Primary beneficiary (institutional/arbitrage) — captures increased revenue and brand lock-in through salt-enhanced products
 *   - Sodium-Sensitive Populations: Primary victim (powerless/trapped) — health conditions make exit costly; bear full extraction burden
 *   - Health-Conscious Consumers: Secondary victim (moderate/constrained) — constrained by product availability and marketing saturation; benefit from brand familiarity but pay extraction cost through hidden sodium
 *   - Regulatory Agencies (FDA): Organized actors (organized/constrained) — see salt strategy as temporary coordination failure with policy sunset; constrained by industry lobbying and regulatory capture
 *   - Competing Chocolate Manufacturers: Institutional actors (institutional/constrained) — maintain high-sodium formulas through competitive imitation; trapped in industry norm equilibrium
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing profit-driven sodium strategy as inevitable human biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hershey_salt_strategy, 0.58).
domain_priors:suppression_score(hershey_salt_strategy, 0.62).
domain_priors:theater_ratio(hershey_salt_strategy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hershey_salt_strategy, extractiveness, 0.58).
narrative_ontology:constraint_metric(hershey_salt_strategy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hershey_salt_strategy, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hershey_salt_strategy, snare).
narrative_ontology:human_readable(hershey_salt_strategy, "Hershey's Salt Inclusion Strategy").
narrative_ontology:topic_domain(hershey_salt_strategy, "economic/food_manufacturing").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hershey_salt_strategy, hershey_shareholders).
narrative_ontology:constraint_beneficiary(hershey_salt_strategy, hershey_marketing_division).
narrative_ontology:constraint_victim(hershey_salt_strategy, health_conscious_consumers).
narrative_ontology:constraint_victim(hershey_salt_strategy, sodium_sensitive_populations).
narrative_ontology:constraint_victim(hershey_salt_strategy, nutritional_label_readers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SODIUM-SENSITIVE CONSUMER (SNARE) — Trapped by health condition (hypertension, kidney disease) and the ubiquity of Hershey products in retail landscape. Cannot easily exit the product category without significant dietary sacrifice. Maximum extraction as health burden increases while consumer belief in 'treating themselves' with chocolate persists despite rising sodium content. No exit option.
constraint_indexing:constraint_classification(hershey_salt_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTH-CONSCIOUS CONSUMER (TANGLED ROPE) — Constrained by product availability, marketing saturation, and the coordination function that Hershey's enjoys as a dominant chocolate producer. Benefits from convenient access to familiar products and brand trust; bears extraction through manipulated flavor expectations and hidden sodium accumulation. Partial exit via premium brands, but constraint persists due to price and availability barriers.
constraint_indexing:constraint_classification(hershey_salt_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HERSHEY CORPORATION & SHAREHOLDERS (ROPE) — Experiences salt strategy as coordination solution: salt increases hedonic response, reduces water activity (extends shelf life), lowers manufacturing costs through reduced cocoa requirements, and strengthens brand lock-in through habit formation. Extraction flows toward this agent. Arbitrage exit available (can shift strategy at any time). Net beneficiary of the constraint.
constraint_indexing:constraint_classification(hershey_salt_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY & PUBLIC HEALTH ADVOCATES (SCAFFOLD) — Organized agents (FDA sodium guidelines, public health campaigns, nutrition labeling mandates) see the salt strategy as a temporary coordination failure with a structural sunset. Sodium reduction policies, trans-fat restrictions precedent, and consumer awareness campaigns are building alternative incentives. However, current constraint persists because regulatory capture and industry lobbying maintain permissive sodium thresholds. Sunset depends on policy change and consumer coalition formation.
constraint_indexing:constraint_classification(hershey_salt_strategy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPETITIVE CHOCOLATE INDUSTRY STANDARDS (PITON) — The salt strategy propagated across the industry is now maintained by competitive imitation rather than functional necessity. Rival manufacturers adopted salt-heavy formulas to match Hershey's market dominance, creating a norm lock-in. The theater ratio reflects that sodium content now serves as marketing signal ('enhanced flavor') rather than genuine consumer preference — manufacturers maintain the practice through competitive inertia despite declining health narrative. Degraded constraint maintained by industry convention.
constraint_indexing:constraint_classification(hershey_salt_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a chemical/biological perspective, salt enhancement of hedonic response is an immutable property of human sensory systems: sodium ions activate specific taste receptors and trigger pleasure pathways independent of consumer preference or policy. This perspective naturalizes the strategy as following inherent human biology. However, this conflates mechanism (salt enhances taste) with constraint (systematic, profitable, asymmetric deployment of that mechanism). The engine's false summit detector reveals the naturalization — the constraint is not the fact of salt's taste properties, but the strategic decision to maximize salt while obscuring sodium content from health-conscious consumers.
constraint_indexing:constraint_classification(hershey_salt_strategy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hershey_salt_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hershey_salt_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hershey_salt_strategy, TR),
    TR >= 0.70.

:- end_tests(hershey_salt_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Hershey captures significant revenue and brand lock-in through salt-enhanced demand while shifting health costs to consumers. The extraction is not maximal (0.70+) because exit options exist (premium brands, homemade alternatives, dietary adjustment) and suppression is structural rather than total — consumers retain agency to modify behavior if they internalize health costs. The trajectory shows extractiveness rising from 0.32 (early strategy, limited rollout) to 0.58 (saturated product line, normalized consumption) as the strategy penetrated the market. Suppression (0.62): Moderate-high. Barriers include marketing framing that obscures sodium risk, limited consumer awareness of cumulative intake, product ubiquity in retail and vending, and deliberate targeting of convenience-dependent populations. Suppression is structural but not total — labeling requirements and nutrition education reduce it. Theater ratio (0.48): Moderate. The strategy initially had functional justification (salt as flavor enhancer and preservative), but over 20 years the performative component grew as 'enhanced flavor' marketing replaced genuine product improvement. The ratio increased from 0.25 to 0.48, reflecting that the narrative ('premium indulgence') now obscures the mechanism (salt as cheap volume enhancer).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival gap between beneficiary and victim. Hershey experiences the salt strategy as coordination — a solution to consumer demand for enhanced flavor that also improves manufacturing efficiency. The strategy aligns beneficiary incentives with product quality (to them). Sodium-sensitive consumers experience it as pure extraction — they cannot exit, they bear all health costs, and the strategy directly targets their hedonic weaknesses. The gap widens because marketing framing ('premium,' 'indulgence') appeals to beneficiary narratives while obscuring victim narratives (health risk, sodium burden). The regulatory perspective shows a temporal gap: in the immediate (institutional/immediate/arbitrage/national), regulators see rope (coordination via voluntary industry guidelines); in the generational (organized/generational/constrained/national), they see scaffold (sunset via policy change). The piton perspective reveals a competitive norm lock-in — manufacturers maintain high-sodium formulas not because the strategy is optimal, but because abandoning it would yield market share loss to competitors who maintain the standard. The mountain perspective naturalizes the strategy as inevitable human biology, but structural analysis reveals the naturalization — the constraint is not that salt enhances taste (which is true), but that Hershey systematically deploys this fact to extract rents from trapped consumers while obscuring cumulative intake.
 *
 * DIRECTIONALITY LOGIC:
 *   Hershey shareholders experience low d (0.05-0.15) because they are beneficiaries with arbitrage exit — they profit from the strategy and can exit at any time (shift to low-sodium, reformulate, rebrand). Sodium-sensitive consumers experience high d (0.90-0.95) because they are trapped victims with health conditions that make exit costly — their directionality is near-maximal, producing high f(d) ≈ 1.40, amplifying experienced extractiveness. Health-conscious consumers experience intermediate d (0.55-0.65) because they are constrained victims with partial exit options (can buy competitors, but at higher price or lower availability) — their f(d) ≈ 0.75 produces moderate chi amplification. Regulatory agencies experience intermediate d (0.50-0.60) as mixed beneficiaries and victims — they benefit from industry tax revenue but bear public health costs — producing f(d) ≈ 0.65. The beneficiary perspective (rope) derives d from arbitrage exit + beneficiary status; the snare perspective derives d from trapped exit + victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by showing that Hershey's salt strategy is NOT a false naturalization of coordination. It exhibits genuine snare properties: systematic extraction from trapped consumers who cannot exit due to health conditions. The constraint is also NOT a false extraction masquerading as coordination — the beneficiary's rope perspective is genuine: salt does provide coordination benefits (flavor enhancement, cost reduction, shelf-life extension) alongside extraction. This is the canonical tangled rope case: the same mechanism (salt inclusion) simultaneously provides a coordination service (improved product taste) and extracts rents from health-sensitive populations (obscured sodium burden). The classification resolves to snare from the victim's perspective (trapped consumers), rope from the beneficiary's perspective, scaffold from the regulatory perspective (temporary, with sunset via policy change), and piton from the competitive industry perspective (degraded coordination norm maintained by inertia). No single type captures all perspectives — the presheaf over observation positions IS the full answer. The mandatrophy is resolved by recognizing that the mechanism is genuinely hybrid: coordination that has been systematically weaponized into extraction through targeting and marketing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sodium_label_visibility,
    'Does increased nutritional label transparency reduce sodium extraction effectiveness, or does consumer inattention persist regardless of label clarity?',
    'Comparative analysis of sodium reduction in markets with vs. without front-of-package sodium warnings; consumer behavior studies correlating label visibility with purchase patterns',
    'If transparency reduces extraction: suppression is contingent, not structural, and the constraint degrades. If inattention persists: suppression is near-structural, maintaining snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sodium_label_visibility, empirical, 'Whether label transparency reduces sodium extraction effectiveness').

omega_variable(
    consumer_taste_adaptation,
    'Is the hedonic preference for high-salt chocolate products a genuine adaptation or a manufactured dependency that would reverse with palate re-calibration?',
    'Longitudinal studies of consumers transitioning to low-sodium chocolate; market analysis of reduced-sodium product uptake when marketed with health framing; taste preference surveys across demographic cohorts with different sodium exposure histories',
    'If adaptation (genuine preference): extraction mechanism is more entrenched — consumers actively prefer high-salt. If dependency (manufactured): exit costs are lower and constraint could degrade faster with intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_taste_adaptation, empirical, 'Whether high-salt preference is adaptation or manufactured dependency').

omega_variable(
    regulatory_capture_durability,
    'Are current FDA sodium guidelines structurally captured by the food industry, or are they evidence-based safety thresholds that industry lobbying merely delays revising?',
    'Historical analysis of FDA sodium guideline changes vs. industry testimony; comparison of US limits with other developed nations'' regulations; epidemiological evidence linking current sodium thresholds to health outcomes',
    'If captured: regulatory scaffold perspective is aspirational only, and constraint persists. If evidence-based: sunset mechanism exists and scaffold is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_durability, empirical, 'Whether FDA sodium guidelines are captured or evidence-based').

omega_variable(
    industry_coordination_necessity,
    'Does the salt strategy require active industry coordination (cartel-like behavior), or does competitive dynamics alone drive converge to high-sodium equilibrium?',
    'Antitrust analysis of pricing and formulation changes; historical documentation of industry meetings or lobbying on sodium; market modeling of whether low-sodium competitor strategies would yield market share gains',
    'If coordination required: snare classification weakens (requires active enforcement to maintain extraction). If natural equilibrium: snare is self-enforcing through competition alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_coordination_necessity, empirical, 'Whether salt strategy requires active industry coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hershey_salt_strategy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hershey_salt_tr_t0, hershey_salt_strategy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hershey_salt_tr_t10, hershey_salt_strategy, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hershey_salt_tr_t20, hershey_salt_strategy, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(hershey_salt_be_t0, hershey_salt_strategy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hershey_salt_be_t10, hershey_salt_strategy, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hershey_salt_be_t20, hershey_salt_strategy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hershey_salt_strategy, resource_allocation).
narrative_ontology:affects_constraint(hershey_salt_strategy, processed_food_sodium_targeting).
narrative_ontology:affects_constraint(hershey_salt_strategy, hedonic_product_manipulation).
narrative_ontology:affects_constraint(hershey_salt_strategy, regulatory_capture_food_industry).

% DUAL FORMULATION NOTE:
% The salt strategy is downstream of food manufacturing cost pressures and hedonic design principles, which generate the functional justification for salt inclusion. The strategy is upstream of broader processed food sodium targeting and regulatory capture mechanisms. The ε value (0.58) reflects the constraint as a deliberate market-facing strategy; decomposition would require separate stories for the manufacturing economics (lower ε) and regulatory capture (higher ε) that enable it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hershey_salt_strategy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
