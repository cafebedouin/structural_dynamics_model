% ============================================================================
% CONSTRAINT STORY: sound_money_scarcity_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sound_money_scarcity_constraint, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sound_money_scarcity_constraint
 *   human_readable: Sound Money Scarcity Constraint in Cryptocurrency Markets
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Cryptocurrency presents a structural puzzle: the same technical
 *   commitment (fixed-supply scarcity) is narratively justified as solving
 *   three incoherent problems — Austrian sound money (prevention of
 *   government debasement through mathematical inevitability),
 *   speculation-enabling asset (volatility as market opportunity), and
 *   decentralization ideology (disintermediation from institutional control).
 *   These three framings have fundamentally different beneficiary/victim
 *   structures and success metrics. The scarcity constraint functions as a
 *   Tangled Rope hybrid: it genuinely coordinates around an alternative
 *   monetary legitimacy claim (decentralized store of value, escape from
 *   political monetary policy) while extracting from price-stability seekers,
 *   policy autonomy, and those unable to absorb volatility. The
 *   extractiveness has risen significantly over the measurement interval
 *   (0.32 → 0.58) as institutional adoption and mainstream financialization
 *   have concentrated wealth in early-adopter and mining-infrastructure
 *   hands. The theater ratio has also risen (0.42 → 0.68), indicating that
 *   narrative justifications ('sound money,' 'inevitable decentralization')
 *   increasingly carry the weight previously borne by technical function.
 *   This pattern is consistent with institutional capture: the constraint's
 *   legitimacy increasingly depends on rhetorical repetition rather than
 *   functional coordination. The constraint exhibits a perspectival gap
 *   spanning all six classification types, suggesting either that a contested
 *   kernel is being framed as a unitary constraint (supporting ε-invariance
 *   decomposition into three stories) or that the multiple perspectives
 *   genuinely reflect one hybrid constraint viewed from radically different
 *   structural positions.
 *
 * KEY AGENTS:
 *   - Early Adopters / Protocol Developers: Primary beneficiary (institutional/arbitrage) — capture appreciating token allocation, network-effect rents, and transition to mainstream infrastructure providers
 *   - Mining Infrastructure Operators: Primary beneficiary (institutional/arbitrage) — benefit from scarcity-driven energy costs as coordination mechanism (Proof-of-Work difficulty), and from appreciation of mining rewards
 *   - Retail Participants (Price Stability Seekers): Primary victim (powerless/trapped) — seek price-stable currency or transactions but face maximum volatility and wealth concentration dynamics
 *   - Monetary Policy Autonomy / Central Bank Authorities: Secondary victim (institutional/constrained) — face reduced policy space and legitimacy challenge from alternative monetary narratives
 *   - Developing Nation Currency Users: Secondary beneficiary + victim (moderate/constrained) — benefit from inflation hedge and capital-control escape but face volatility absorption costs and network-lock volatility
 *   - Open Finance Protocol Coalition: Organized agent (organized/mobile) — building alternatives (stablecoins, algorithmic money) that create coordination benefits outside scarcity constraint
 *   - Academic Monetary Theory Establishment: Institutional theater-maintainer (institutional/arbitrage) — perpetuates scarcity-as-legitimacy narratives through publications and institutional authority despite theoretical critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sound_money_scarcity_constraint, 0.58).
domain_priors:suppression_score(sound_money_scarcity_constraint, 0.62).
domain_priors:theater_ratio(sound_money_scarcity_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sound_money_scarcity_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sound_money_scarcity_constraint, tangled_rope).
narrative_ontology:human_readable(sound_money_scarcity_constraint, "Sound Money Scarcity Constraint in Cryptocurrency Markets").
narrative_ontology:topic_domain(sound_money_scarcity_constraint, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(sound_money_scarcity_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sound_money_scarcity_constraint, '2b86acd7-ed31-4f2c-bf99-51f63182dddd').
narrative_ontology:cs_created_at('2b86acd7-ed31-4f2c-bf99-51f63182dddd', '').
narrative_ontology:cs_kernel_codification('2b86acd7-ed31-4f2c-bf99-51f63182dddd', fixed_text).
narrative_ontology:cs_authority_grounding('2b86acd7-ed31-4f2c-bf99-51f63182dddd', distributed).
narrative_ontology:cs_reading_relation('2b86acd7-ed31-4f2c-bf99-51f63182dddd', cryptocurrency_speculation_reading, forecloses).
narrative_ontology:cs_reading_relation('2b86acd7-ed31-4f2c-bf99-51f63182dddd', decentralization_autonomy_reading, coexists_with).
narrative_ontology:cs_axiom('2b86acd7-ed31-4f2c-bf99-51f63182dddd', foundational, mathematical_scarcity_prevents_debasement).
narrative_ontology:cs_axiom_status(mathematical_scarcity_prevents_debasement, holdable).
narrative_ontology:cs_axiom('2b86acd7-ed31-4f2c-bf99-51f63182dddd', secondary, fixed_supply_requires_no_political_discretion).
narrative_ontology:cs_axiom_status(fixed_supply_requires_no_political_discretion, holdable).
narrative_ontology:cs_reference_frame('2b86acd7-ed31-4f2c-bf99-51f63182dddd', austrian_monetary_legitimacy).
narrative_ontology:cs_drift_state('2b86acd7-ed31-4f2c-bf99-51f63182dddd', contemporary_financialization_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, early_adopters).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, protocol_developers).
narrative_ontology:constraint_beneficiary(sound_money_scarcity_constraint, mining_infrastructure).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, retail_participants).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, monetary_policy_autonomy).
narrative_ontology:constraint_victim(sound_money_scarcity_constraint, price_stability_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL PARTICIPANT (SNARE) — Powerless agents seeking price-stable currency for savings or transactions face maximum extraction. Fixed-supply scarcity mechanics concentrate appreciation gains in early-adopter hands; volatility is structurally inherent to the constraint and immobilizes wealth. No exit without abandoning the system entirely. Experiences the constraint as pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION CURRENCY USER (TANGLED ROPE) — Constrained by high barriers to exit (local currency instability, capital controls) but also benefits from access to decentralized store of value outside government seizure. Genuine coordination function (cross-border transfer, inflation hedge) exists alongside asymmetric extraction (volatility risk, network effects concentration). The constraint coordinates monetary autonomy while extracting from those unable to absorb price swings.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTOCOL DEVELOPER (ROPE) — Institutional actors (core developers, early miners) experience the scarcity constraint as pure coordination: fixed supply mechanisms enable network security (proof-of-work difficulty, validator incentives) and establish protocol legitimacy. They benefit from first-mover appreciation, token allocation, and transition to institutional infrastructure providers. Extraction flows toward them, but they perceive this as earned coordination reward.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANK AUTHORITY (TANGLED ROPE) — Constrained by political pressure not to suppress private currencies directly but beneficiary of maintaining monetary policy monopoly. The scarcity constraint coordinates around a alternative legitimacy claim (sound money as escape from fiat debasement) while extracting from central banks' ability to conduct countercyclical policy. Both coordination (alternative monetary standard) and asymmetric extraction (policy space compression) are structurally present.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC MONETARY THEORY (PITON) — The scarcity-as-legitimacy narrative persists in institutional crypto discourse despite substantial theoretical critique (endogenous money, modern monetary theory, currency hierarchy literature). The constraint's theater ratio is high: 'sound money' claims persist through narrative repetition and community identity rather than consistent analytical function. Establishment economists critique the scarcity thesis but the narrative maintains institutional inertia within crypto communities.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN FINANCE COALITION (TANGLED ROPE) — Organized agents (stablecoin issuers, DeFi protocols, decentralized exchanges) are building coordinated alternatives to scarcity-based value (algorithmic stablecoins, fractional-reserve staking, multi-token systems). They experience the scarcity constraint as both coordination mechanism (for base-layer security they depend on) and extraction (from having to work around fixed-supply limitations). Mobile exit options through protocol innovation create genuine agency.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY-AS-NATURAL-LAW (MOUNTAIN) — From a civilizational view, fixed-supply scarcity could appear as a fundamental constraint of currency design: you cannot have both decentralization (no trusted issuer) and flexible money supply simultaneously. This perspective risks naturalizing the scarcity-maximizing design choice as inevitable. However, the structural data contradicts the mountain classification — beneficiary concentration and measured suppression indicate the 'inevitability' is a reading that benefits specific actors, triggering false-summit evaluation.
constraint_indexing:constraint_classification(sound_money_scarcity_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sound_money_scarcity_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sound_money_scarcity_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sound_money_scarcity_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sound_money_scarcity_constraint, TR),
    TR >= 0.70.

:- end_tests(sound_money_scarcity_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate and rising. The scarcity constraint concentrates appreciation gains in early-adopter hands through first-mover advantage, network effects, and token allocation inequality. The measured value reflects that substantial coordination benefits exist (decentralized security, inflation protection, cross-border settlement) but are increasingly captured asymmetrically. The rising trajectory (0.32 → 0.58 over 10 years) reflects institutional adoption concentrating wealth rather than distributing it. Suppression (0.62): High. Multiple suppression mechanisms operate simultaneously: (1) network effects create lock-in (platform switching is costly), (2) institutional integration (exchange custody, financial services) concentrates control, (3) narrative legitimacy ('scarcity is inevitable,' 'decentralization is natural') suppresses consideration of alternatives. Suppression is primarily political/social rather than computational, but substantial enough to prevent easy coalition exit. Theater ratio (0.68): High and rising. The increasing gap between narrative justifications ('sound money') and measured outcomes (speculation, wealth concentration, price volatility) indicates that the constraint's legitimacy increasingly depends on rhetorical repetition. Early periods (t=0) relied more on technical novelty and genuine protocol innovation; recent periods (t=8-10) show increased reliance on ideological narrative, institutional adoption announcements, and regulatory arbitrage claims to maintain legitimacy. This rising theater is consistent with Piton dynamics (former innovation becoming institutional inertia), though the Tangled Rope base extractiveness remains high enough to classify as active constraint rather than degraded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full DR range across institutional contexts. The protocol developer sees pure coordination (Rope) — scarcity funds security incentives. The early adopter sees appreciation opportunity (Rope beneficiary framing). The retail price-seeker sees pure extraction (Snare) — volatility immobilizes wealth. The developing nation user sees mixed coordination and extraction (Tangled Rope) — escape from local currency instability exists alongside volatility absorption. The central bank sees policy space compression (Tangled Rope, treating the constraint as a victim). The open finance coalition sees constrained coordination with mobile exit (Tangled Rope with organized agent power, shifting toward Rope as alternatives mature). The civilizational analytical observer risks seeing scarcity as inevitable monetary design (Mountain), but the structural data (rising extractiveness, measured beneficiary concentration, performing narrative legitimacy) contradicts the naturalization. This perspectival spread strongly suggests that 'sound money scarcity' is conflating three distinct constraints with different ε values and victim/beneficiary structures, recommending decomposition into separate constraint stories per the ε-invariance principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the extraction flow and their exit options. Early adopters (beneficiary + arbitrage) derive low or negative d values — appreciation concentrates toward them. Powerless retail participants (victim + trapped) derive high d values — they experience extraction with no exit. Developing nation users (mixed victim/beneficiary + constrained) derive mid-range d values reflecting both coordination benefits and volatility costs. Institutional actors (central banks, protocol developers) bifurcate: developers/miners with arbitrage exit get low d; central banks with constrained exit but victim status get moderate-high d. The open finance coalition (organized + mobile) gets moderate d values because they have agency and exit options. The analytical observer at civilizational scope derives high d because the naturalization ('this is how money must work') suppresses visibility of the beneficiary structure and extraction mechanisms. No directionality overrides are needed — the standard derivation chain from beneficiary/victim declarations produces coherent d values across contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is not 'which type is correct?' but 'are three incoherent readings being collapsed into one constraint label?' Strong indicators that ε-invariance decomposition is warranted: (1) The three narrative framings (sound money, speculation, decentralization) have fundamentally different success metrics and beneficiary structures. (2) Early adopters benefit from speculation and scarcity; sound money advocates seek price stability (contradictory); decentralization advocates seek disintermediation but accept institutional infrastructure (contradictory). (3) The ε values would diverge substantially by framing: pure-sound-money reading ≈ 0.25 (low extraction, genuine monetary coordination), pure-speculation reading ≈ 0.72 (high extraction, minimal coordination), decentralization reading ≈ 0.42 (mixed coordination and constraint). The unified story (ε = 0.58, Tangled Rope) is an average that obscures the structural incoherence. RECOMMENDATION: Decompose into three constraint stories: (1) sound_money_coordination (ε ≈ 0.25, Rope), (2) cryptocurrency_speculation_extraction (ε ≈ 0.72, Snare), (3) decentralization_monetary_autonomy (ε ≈ 0.42, Tangled Rope). Link via network.affects_constraints. The current unified story should transition to a kernel-reading analysis documenting which reading is being emphasized in specific institutional contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_vs_coordination_function,
    'Does cryptocurrency''s fixed-supply scarcity function as a genuine security mechanism (proof-of-work incentive alignment, Sybil resistance) or is it primarily a wealth-concentration device (appreciating store of value for early adopters)?',
    'Ablation analysis: compare security outcomes in systems with graduated supply curves vs fixed-maximum supply; examine correlation between scarcity narratives and adoption patterns in alternative-supply cryptocurrencies; analyze whether network effect benefits accrue primarily to scarcity advocates or are orthogonal to supply design',
    'If primarily security: classification shifts toward Rope for broader agent classes (scarcity genuinely coordinates). If primarily wealth-concentration: Snare classification spreads (extraction is the mechanism, security claims are theater). Could change institutional perspective from Rope to Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_vs_coordination_function, empirical, 'Whether scarcity is security mechanism or wealth-concentration device').

omega_variable(
    decentralization_monetary_policy_tradeoff,
    'Is the elimination of monetary policy discretion (through fixed supply) a genuine benefit (escape from political capture, inflation prevention) or a structural constraint that prevents countercyclical policy and economic stabilization?',
    'Historical comparison: macroeconomic stability outcomes in periods using commodity-backed currencies vs floating-supply regimes; analysis of depressionary episodes in Bitcoin history vs coordinated policy responses in fiat systems; counterfactual modeling of 2008-2020 crisis periods under fixed-supply constraint',
    'If benefit dominates: central bank authority perspective may shift to Scaffold (temporary monopoly being displaced). If constraint dominates: central bank extraction picture clarifies (the constraint extracts policy autonomy). Could cascade to developing nation and retail perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_monetary_policy_tradeoff, empirical, 'Tradeoff between monetary policy discretion and scarcity-constraint security').

omega_variable(
    kernel_vs_three_distinct_constraints,
    'Are ''sound money'' (Austrian), ''speculative asset'' (financialization), and ''decentralization'' (cypherpunk) three readings of one contested kernel (the legitimacy claim of cryptocurrency itself), or three structurally distinct constraints that have been conflated under one label?',
    'Coherence-boundary testing: can a single agent simultaneously hold all three readings in a consistent framework? (Unlikely — sound money ideology contradicts pure speculation; decentralization contradicts institutional financial integration.) Do the three readings have shared beneficiaries? (No — speculators benefit from volatility; sound money advocates seek price stability.) Do they have logically foreclosing axioms? (Yes — sound money assumes scarcity = legitimacy; speculation assumes volatility = opportunity; decentralization assumes disintermediation = governance autonomy. Each forecloses aspects of the others.)',
    'If three readings of one kernel: generate three separate constraint stories, link via network.affects_constraints, each with its own ε and perspectives. This is the ε-invariance decomposition case. If one constraint: maintain current unified story. Current assessment: strong evidence for decomposition — ε values would differ substantially (sound money ε ≈ 0.35 for coordination, speculation ε ≈ 0.72 for extraction, decentralization ε ≈ 0.42 for coordination-plus-suppression). Recommend decomposition into three kernel-reading stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_three_distinct_constraints, conceptual, 'Whether ''sound money'' is one contested kernel or three conflated constraints').

omega_variable(
    suppression_mechanism_legitimacy,
    'Is the suppression measured (0.62) structural (real barriers: computational costs, institutional barriers to exit, network effects lock-in) or performative (narrative suppression: ''decentralization is inevitable,'' ''scarcity is natural,'' eliminating alternative framings without material barriers)?',
    'Mechanism analysis: can retail participants or policy autonomy advocates exit the constraint via coordinated action (platform shift, regulation, alternative protocols)? (Yes, with coordination costs.) Are the barriers mathematical/computational or political/social? (Primarily political/social — network effects, institutional adoption.) Does suppression magnitude change when narrative legitimacy is challenged? (Substantial evidence: regulatory clarity, institutional integration announcements create volatility spikes that reduce retail suppression temporarily.)',
    'If structurally suppressive: tang metrics stand. If primarily narrative: revise suppression downward (0.62 → 0.48) and increase theater (0.68 → 0.76), moving classification toward Piton for several perspectives. Affects retail, developing nation, and policy autonomy victim designations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_legitimacy, empirical, 'Whether suppression is structural or performative').

omega_variable(
    early_adopter_rent_sustainability,
    'Can early-adopter wealth concentration (the primary extraction mechanism benefiting protocol developers and miners) persist indefinitely, or will institutional integration and market maturation redistribute rents toward later adopters and exchange/custody providers?',
    'Time-series analysis of wealth concentration (Gini coefficient of token distribution); tracking of whose institutional adoption (exchanges, custodians, financial services, central banks) accrues rents to; comparison with historical precedent in commodity markets, equity markets, and prior monetary revolutions (fiat adoption, credit card networks)',
    'If persistent: scarcity-based rent extraction is structural (Snare classification confirmed). If redistributing: extraction is declining (Tangled Rope shifting to Rope for later-adopter institutional actors). Affects beneficiary and victim group stability over measurement interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_adopter_rent_sustainability, empirical, 'Sustainability of early-adopter rent concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sound_money_scarcity_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soundmoney_tr_t0, sound_money_scarcity_constraint, theater_ratio, 0, 0.42).
narrative_ontology:measurement(soundmoney_tr_t4, sound_money_scarcity_constraint, theater_ratio, 4, 0.58).
narrative_ontology:measurement(soundmoney_tr_t8, sound_money_scarcity_constraint, theater_ratio, 8, 0.68).
narrative_ontology:measurement(soundmoney_tr_t10, sound_money_scarcity_constraint, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(soundmoney_be_t0, sound_money_scarcity_constraint, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(soundmoney_be_t4, sound_money_scarcity_constraint, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(soundmoney_be_t8, sound_money_scarcity_constraint, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(soundmoney_be_t10, sound_money_scarcity_constraint, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sound_money_scarcity_constraint, resource_allocation).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, proof_of_work_security_incentive).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, network_effect_lock_in).
narrative_ontology:affects_constraint(sound_money_scarcity_constraint, monetary_policy_monopoly_challenge).

% DUAL FORMULATION NOTE:
% This constraint story currently presents a unified average (ε = 0.58, Tangled Rope) across three structurally distinct readings of cryptocurrency legitimacy. Strong evidence suggests ε-invariance decomposition into three separate constraint stories: (1) sound_money_coordination (ε ≈ 0.25, Rope, Austrian framing), (2) cryptocurrency_speculation_extraction (ε ≈ 0.72, Snare, financialization framing), (3) decentralization_monetary_autonomy (ε ≈ 0.42, Tangled Rope, cypherpunk framing). Each story should be authored separately with its own perspectives, beneficiary/victim structures, and measurements. They should link via network.affects_constraints to indicate constraint family relationships. The current unified story serves as a diagnostic exemplar of why constraint decomposition is necessary — attempting to model three incoherent commitments under one label produces a Tangled Rope classification that obscures the mandatrophy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
