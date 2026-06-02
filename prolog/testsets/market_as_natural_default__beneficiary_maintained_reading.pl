% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Maintained Beneficiary Constraint (Beneficiary-Maintained Reading)
 *   domain: political_economy/institutional_theory/market_design
 *
 * SUMMARY:
 *   This constraint models market dominance as an actively maintained
 *   extraction mechanism rather than a natural economic law. The
 *   beneficiary-maintained reading interprets 'the market' as a set of legal,
 *   institutional, and informational structures whose perpetuation benefits
 *   incumbent capital holders and financial intermediaries. The constraint
 *   operates through suppression mechanisms including: regulatory capture
 *   (favorable treatment of incumbent firms in licensing, zoning,
 *   environmental review), intellectual property thickets (patent hoarding,
 *   trademark enforcement), infrastructure gatekeeping (credit access,
 *   payment systems, logistics networks), legal barriers to alternatives
 *   (cooperative and mutual aid systems face licensing and capital adequacy
 *   restrictions incumbent firms do not), and predatory acquisition of
 *   disruptors. The high suppression value (0.72) reflects that exiting the
 *   dominant market structure requires not just finding an alternative but
 *   overcoming active legal and economic barriers. The rising trajectory of
 *   both extractiveness and suppression over the measurement interval (40
 *   years) models the ratcheting effect: as disruptive technologies and
 *   organizing movements threaten incumbent dominance, incumbent actors
 *   deploy more sophisticated legal engineering and regulatory lobbying to
 *   maintain their position. This reading coexists with alternatives: a
 *   genuine natural reading sees markets as inherent to human economic
 *   coordination; a lapsed alternative reading documents how market-dominant
 *   structures replace prior cooperative or gift economies through
 *   institutional capture rather than superior efficiency.
 *
 * KEY AGENTS:
 *   - Incumbent Capital Holders: Beneficiary (institutional/arbitrage) — profit from market structure that restricts capital access to elite players. Lobby to maintain barriers.
 *   - Financial Intermediaries: Beneficiary (institutional/arbitrage) — banking, venture capital, insurance derive profit and power from gatekeeping capital allocation. Actively defend credit gatekeeping and collateral requirements.
 *   - Precariat Workers: Victim (powerless/trapped) — dependent on labor markets controlled by capital holders. Cannot exit into alternative systems due to asset poverty and legal barriers to cooperative organizing.
 *   - Market Entrants & Disruption Firms: Victim (powerful/mobile) — have resources to challenge incumbents but face suppression mechanisms (predatory acquisition threats, regulatory capture, infrastructure denial). High power + high extraction = high chi.
 *   - Alternative Economic Systems Coalition: Victim/Organized opponent (organized/constrained) — cooperatives, mutual aid, labor unions face legal and credit barriers incumbent market firms do not. Can coordinate but face asymmetric suppression.
 *   - Free Market Ideology Institution: Beneficiary supporter (institutional/arbitrage) — maintains narrative that market dominance is natural/inevitable, masking suppression mechanisms. Provides rhetorical shield for incumbent interests.
 *   - Analytical Observer: Sees full structure (analytical/analytical) — risks accepting naturalizing narrative that market dominance is inherent rather than maintained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.68).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.72).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Dominance as Actively Maintained Beneficiary Constraint (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/institutional_theory/market_design").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '32230dd7-7542-4fa6-a571-abe6b4f72c0c').
narrative_ontology:cs_kernel_codification('32230dd7-7542-4fa6-a571-abe6b4f72c0c', distributed).
narrative_ontology:cs_authority_grounding('32230dd7-7542-4fa6-a571-abe6b4f72c0c', extraction).
narrative_ontology:cs_interpretation_layer_present('32230dd7-7542-4fa6-a571-abe6b4f72c0c').
narrative_ontology:cs_reading_relation('32230dd7-7542-4fa6-a571-abe6b4f72c0c', market_as_natural_default__genuine_natural_reading, coexists_with).
narrative_ontology:cs_reading_relation('32230dd7-7542-4fa6-a571-abe6b4f72c0c', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_axiom('32230dd7-7542-4fa6-a571-abe6b4f72c0c', foundational, market_dominance_actively_maintained).
narrative_ontology:cs_axiom_status(market_dominance_actively_maintained, holdable).
narrative_ontology:cs_axiom_grounding('32230dd7-7542-4fa6-a571-abe6b4f72c0c', market_dominance_actively_maintained, empirically_contingent).
narrative_ontology:cs_axiom('32230dd7-7542-4fa6-a571-abe6b4f72c0c', foundational, suppression_mechanisms_deliberately_constructed).
narrative_ontology:cs_axiom_status(suppression_mechanisms_deliberately_constructed, holdable).
narrative_ontology:cs_axiom_grounding('32230dd7-7542-4fa6-a571-abe6b4f72c0c', suppression_mechanisms_deliberately_constructed, empirically_contingent).
narrative_ontology:cs_reference_frame('32230dd7-7542-4fa6-a571-abe6b4f72c0c', market_dominance_through_beneficiary_extraction).
narrative_ontology:cs_drift_state('32230dd7-7542-4fa6-a571-abe6b4f72c0c', contemporary_regulatory_capture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32230dd7-7542-4fa6-a571-abe6b4f72c0c', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_intermediaries).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, monopoly_positioned_firms).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, market_entrants).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, alternative_economic_systems).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, precariat_labor).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, consumer_choice_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIAT WORKER (SNARE) — Trapped by capital scarcity and employment dependency. Cannot exit the dominant labor market structure. Suppression is structural: debt obligations, lack of asset ownership, geographic immobility, and legal barriers to cooperative alternatives. Experiences maximum extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTERNATIVE SYSTEMS COALITION (TANGLED ROPE) — Organized actors (cooperative networks, mutual aid structures, labor unions, postcapitalist experimenters) see genuine coordination function: markets do coordinate distributed information and enable voluntary exchange. But extraction layer is real: legal barriers, credit access restriction, regulatory capture force cooperative models to compete on hostile terrain. Not snare because the coalition has agency; not rope because extraction is unmistakable.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT FINANCIAL INTERMEDIARIES (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: market dominance enables their role as gatekeepers of capital allocation. Banking system, venture capital, insurance derive institutional function from market structure. Suppression mechanisms (regulatory capture, credit rating gatekeeping, collateral requirements) appear to them as legitimate risk management. Low or negative experienced extraction — they ARE the extraction mechanism.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET ENTRANTS & DISRUPTION FIRMS (SNARE) — Powerful actors with capital and technology see extraction clearly: incumbent lobbying against platform regulation, intellectual property thickets, predatory acquisition threats, and access denial to critical infrastructure (cloud computing, payment networks, logistics). These firms have agency and resources but face suppression mechanisms specifically engineered to maintain incumbent dominance. Chi rises sharply because their power amplifies the extraction they perceive — the incumbents target them with concentrated suppression.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FREE MARKET IDEOLOGY INSTITUTION (PITON) — Theater ratio (0.64) reflects: markets are described as 'natural,' 'efficient,' 'self-correcting,' yet actual market structure depends entirely on legal engineering, property law, bankruptcy law, corporate charter law, and active enforcement. The ideological story (markets emerge naturally) masks the suppression mechanisms (regulatory capture, legal barriers, credit gatekeeping). The institution persists through inertia — the ideological narrative protects beneficiaries from scrutiny.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational scope, markets could be framed as natural: human exchange, specialization, and division of labor are foundational to any complex economy. This perspective sees incumbent dominance as an immutable consequence of market structure itself — 'winner-take-most dynamics are inherent.' However, the structural data reveals false summit: the 'naturalness' is fabricated through decades of legal engineering, regulatory capture, and active suppression of alternatives. The constraint is maintained, not natural.
constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_as_natural_default__beneficiary_maintained_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, TR),
    TR >= 0.70.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Incumbent beneficiaries extract substantial value through: (1) pricing power protected by market dominance, (2) capital returns amplified by barriers to competitive entry, (3) labor extraction enabled by precariat dependency on dominant employers, (4) monopoly rents on network effects (data, infrastructure). The value reflects that extraction is the central mechanism beneficiaries use to maintain dominance — without extraction margin, they would compete on efficiency and innovation rather than suppression. Suppression (0.72): High. Exiting the dominant market structure requires overcoming multiple coordinated barriers: capital access (credit scoring gatekeeping), legal structures (corporate law favoring incumbents, cooperative law restricting alternatives), infrastructure (payment systems, cloud computing, logistics networks owned or controlled by incumbents), labor law (restrictions on cooperative organizing and worker ownership), and intellectual property (patent thickets, trade secrets). These barriers are actively maintained — lobbying expenditures, regulatory agency personnel cycling, legislative capture are all observable mechanisms. Theater ratio (0.64): Moderate-high. Markets are described as 'natural,' 'efficient,' 'self-correcting,' yet actual market structure depends entirely on legal engineering. The performative content: free-market rhetoric masks regulatory capture; 'consumer choice' masks restricted alternatives; 'innovation' masks incumbents acquiring disruption threats; 'risk management' masks credit gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspective (institutional/arbitrage) sees rope or coordination — markets enable distributed exchange and capital allocation. The precariat victim (powerless/trapped) sees snare — no exit options due to capital scarcity and legal barriers. Market entrants (powerful/mobile) see snare with higher chi — they have resources but face concentrated suppression. Alternative systems coalition (organized/constrained) sees tangled rope — genuine coordination function (market enables exchange) but extraction layer (barriers to alternatives) is unmistakable. The free market ideology institution (piton) sees performative market efficiency — the theater masks the suppression mechanisms. The analytical observer risks seeing mountain (markets are natural) but the structural data reveals false summit: decades of legal engineering, regulatory capture, and active lobbying maintain market dominance against alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from agent structural position: beneficiary status (d low), victim status (d high), power level, and exit options. Incumbent beneficiaries with arbitrage exit options have d ≈ 0.15 → f(d) ≈ -0.01 → negative effective extraction (they are benefiting). Precariat workers with trapped exit have d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Market entrants with mobile exit but powerful status have d ≈ 0.55 → f(d) ≈ 0.75 → moderate-high extraction. The organized coalition with constrained exit has d ≈ 0.50 → f(d) ≈ 0.65 → moderate extraction. The scope modifier σ(S) = 1.0 (national scope, standard). The snare classification requires χ ≥ 0.66, which is satisfied when powerful victims face concentrated suppression and moderate baseline extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The beneficiary-maintained reading prevents confusion between market coordination and market extraction. A naïve reading might classify this as rope (markets coordinate!) or even mountain (markets are natural!). The beneficiary-maintained reading correctly identifies: (1) there is genuine coordination function (markets do distribute information and enable exchange); (2) there is asymmetric extraction (incumbent beneficiaries extract rents through suppression mechanisms); (3) suppression is active and maintained (not passively inherent). This is the defining signature of snare from the victim perspective and tangled rope from the alternative systems perspective. The mandatrophy resolution is that no single type is 'correct' — the presheaf of perspectives reveals the constraint's structure. Snare from powerless/trapped. Tangled rope from organized/constrained. Rope from beneficiary/arbitrage. Piton from ideological actor seeing performative market efficiency. Mountain from analytical observer mistakenly naturalizing. The collection of perspectives disambiguates what appeared to be a single constraint into its true structure: extraction masked by coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is market dominance a genuine natural feature of exchange economics, or a contingent institutional arrangement actively maintained by legal engineering?',
    'Historical counterfactual analysis: examine periods and jurisdictions where market structure was altered by legal reform; correlation analysis between suppression mechanisms and incumbent survival; comparison to alternative economic systems'' survival rates under equivalent legal protection.',
    'If genuinely natural: beneficiary-maintained reading is false; constraints should classify as mountain. If contingent and maintained: beneficiary-maintained reading is correct; classification as snare/tangled_rope is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether market dominance is natural or actively maintained').

omega_variable(
    suppression_mechanism_intentionality,
    'Are regulatory barriers to alternative systems (licensing requirements, capital adequacy rules, antitrust enforcement selectivity) designed deliberately to protect incumbents, or do they emerge as unintended byproducts of legitimate safety regulation?',
    'Legislative history analysis; comparative regulation study across jurisdictions with different political economies; interview data from regulators and incumbent lobbyists documenting intent vs. stated purpose; counterfactual: what would regulation look like if alternatives were given equal legal standing?',
    'If deliberate: beneficiary-maintained reading is strongly supported; snare classification is appropriate. If unintended: classification may shift toward tangled_rope (coordination with incidental extraction) or scaffold (temporary barriers being corrected).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intentionality, empirical, 'Whether suppression of alternatives is intentional or incidental').

omega_variable(
    alternative_system_viability_ceiling,
    'Could cooperative, mutual aid, or other non-market economic systems survive and scale under conditions of genuine legal parity with incumbent capitalism?',
    'Comparative analysis of alternative systems'' growth under different regulatory conditions; case studies where legal barriers were removed; scaling studies of postcapitalist experiments; examination of why cooperative sectors (agriculture, housing, finance in some jurisdictions) persist despite suppression.',
    'If viable under parity: the constraint''s high extractiveness (0.68) is accurate — incumbent dominance depends on suppression. If not viable: extractiveness may be overstated, and the constraint is less snare and more natural-law-adjacent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_system_viability_ceiling, empirical, 'Viability of alternative systems under legal parity').

omega_variable(
    beneficiary_intentionality_variation,
    'Do all incumbent beneficiaries consciously maintain market dominance through suppression (true snare), or is suppression an emergent property of their individual self-interest without coordinated intent?',
    'Network analysis of lobbying coalitions and regulatory capture patterns; game-theoretic modeling of incumbent incentives; interview data and documentary evidence of explicit coordination vs. parallel interests; differentiation between coordinated cartels and uncoordinated beneficiary behavior.',
    'If coordinated suppression: classification as snare (with organized extraction) is appropriate. If emergent from uncoordinated self-interest: classification may shift toward tangled_rope (coordination function with emergent extraction). This affects assessment of whether suppression mechanisms can be reformed or require structural dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality_variation, conceptual, 'Whether beneficiary suppression is coordinated or emergent from individual incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mktnat_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mktnat_tr_t20, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(mktnat_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(mktnat_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mktnat_be_t20, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mktnat_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mktnat_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mktnat_su_t20, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(mktnat_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__genuine_natural_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, regulatory_capture_mechanism).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, intellectual_property_gatekeeping).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, credit_access_restriction).

% DUAL FORMULATION NOTE:
% The beneficiary-maintained reading is a sibling constraint to the genuine_natural_reading and lapsed_alternative_reading. All three share the same kernel ('market_as_natural_default') but instantiate different constraint types with different ε values because they answer different structural questions. The beneficiary-maintained reading (this file, ε=0.68, snare) models extraction maintained by incumbents. The genuine_natural_reading (separate file) would model markets as inherent to economic coordination (ε≤0.25, mountain or rope). The lapsed_alternative_reading (separate file) would model how alternatives were displaced (ε varies, likely snare or tangled_rope). Network links show downstream constraints that depend on market dominance: regulatory capture (how incumbents maintain barriers), IP gatekeeping (how they restrict competition), credit restriction (how they control capital allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
