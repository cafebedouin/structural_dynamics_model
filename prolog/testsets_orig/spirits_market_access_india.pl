% ============================================================================
% CONSTRAINT STORY: spirits_market_access_india
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spirits_market_access_india, []).

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
 *   constraint_id: spirits_market_access_india
 *   human_readable: Spirits Market Access Constraint in India
 *   domain: economic/regulatory/trade
 *
 * SUMMARY:
 *   India's spirits market operates under state-level licensing monopolies
 *   with excise taxation rates of 150-200%, creating a hybrid
 *   coordination-extraction constraint. The system ostensibly serves public
 *   health and temperance objectives aligned with Gandhian values, but
 *   structurally functions as protected rent-capture for state governments
 *   and incumbent domestic producers. Foreign spirits importers face
 *   systematic exclusion; consumers experience restricted access codified
 *   through state licensing; informal markets (country liquor, illicit
 *   distillation) operate entirely outside the formal regulatory apparatus.
 *   The constraint exhibits tangled rope characteristics: genuine
 *   coordination of tax collection and supply chain management coexists with
 *   asymmetric extraction of monopoly rents and protectionist barriers
 *   against foreign competition. WTO pressure to liberalize market access
 *   introduces sunset logic, making this a potential scaffold constraint with
 *   a generational time horizon.
 *
 * KEY AGENTS:
 *   - Foreign Spirits Importers: Primary victims (powerless/trapped) — face state-level license monopolies, prohibitive tariffs, and systematic exclusion with no exit except market abandonment
 *   - State Governments (Excise Authorities): Primary beneficiaries (institutional/arbitrage) — benefit from licensing monopoly structure, excise revenue (spirits contribute 10-15% of state excise revenue in major states), and administrative control
 *   - Incumbent Domestic Spirits Producers: Secondary beneficiary (moderate/constrained) — protected from foreign competition but dependent on regulatory framework; entrenched but constrained within it
 *   - Prohibition-Aligned Indian Consumers: Secondary victim (moderate/identity_locked) — structurally mobile (can purchase in other states or from informal markets) but identity-locked through Gandhian/temperance cultural narratives making formal purchase socially and morally visible
 *   - International Trade Bodies (WTO/EU/US): Organized challenger (organized/constrained) — view restriction as protectionist extraction and TRIPS violation; constrained by multilateral framework
 *   - Informal Spirits Market (Country Liquor/Illicit Distillation): Underground beneficiary/victim complex — completely unregulated, captures consumer demand that formal system excludes but exposed to quality/health risks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spirits_market_access_india, 0.58).
domain_priors:suppression_score(spirits_market_access_india, 0.65).
domain_priors:theater_ratio(spirits_market_access_india, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spirits_market_access_india, extractiveness, 0.58).
narrative_ontology:constraint_metric(spirits_market_access_india, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(spirits_market_access_india, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spirits_market_access_india, tangled_rope).
narrative_ontology:human_readable(spirits_market_access_india, "Spirits Market Access Constraint in India").
narrative_ontology:topic_domain(spirits_market_access_india, "economic/regulatory/trade").

domain_priors:requires_active_enforcement(spirits_market_access_india).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spirits_market_access_india, state_governments).
narrative_ontology:constraint_beneficiary(spirits_market_access_india, incumbent_domestic_producers).
narrative_ontology:constraint_beneficiary(spirits_market_access_india, excise_revenue_collectors).
narrative_ontology:constraint_victim(spirits_market_access_india, foreign_spirits_importers).
narrative_ontology:constraint_victim(spirits_market_access_india, consumer_choice).
narrative_ontology:constraint_victim(spirits_market_access_india, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Foreign spirits importers face state-level regulatory barriers (license monopolies, prohibitive excise rates 150-200%), no meaningful domestic representation, and systematic exclusion via discriminatory taxation. Exit options are limited to cessation of market participation. Maximum extraction — the constraint exists specifically to prevent this agent's entry.
constraint_indexing:constraint_classification(spirits_market_access_india, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Incumbent domestic producers benefit from protected market access (tariffs, licensing barriers) and price insulation from foreign competition. They also depend on the same regulatory infrastructure for their own licensing and distribution. They experience the constraint as coordination (organizing supply chains, managing licensing) plus extraction (protecting rents from competition). Moderate power with constrained exit — abandoning the protected framework would expose them to international competition but they are organizationally entrenched.
constraint_indexing:constraint_classification(spirits_market_access_india, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% State governments benefit from the licensing monopoly structure (excise revenue, administrative control) and experience the constraint primarily as coordination: managing license allocation, collecting taxes, organizing state-level monopoly spirits shops. Excise revenue from spirits contributes significantly to state coffers. They have arbitrage options (could liberalize but choose not to for revenue reasons). Net beneficiary experiencing coordination function.
constraint_indexing:constraint_classification(spirits_market_access_india, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Indian consumers in regions with strong Gandhian/temperance identity narratives experience spirits access through identity-locked framing rather than material barriers. Many can structurally access spirits but their identity as abstinent/culturally traditional makes exit from the prohibition identity unthinkable. The constraint locks through internalized cultural values, not legal barriers. The state's restrictive framework reinforces identity-locked suppression by making purchase socially visible and morally coded.
constraint_indexing:constraint_classification(spirits_market_access_india, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% International trade bodies and foreign governments (especially EU, US) see India's spirits market restrictions as TRIPS/tariff-code violations and protectionist extraction. They experience the constraint as hybrid: genuine coordination (harmonizing trade regimes) plus asymmetric extraction (India's monopoly rents extracted from foreign producers). Organized power with constrained exit from the multilateral framework. Classification remains tangled_rope because they both benefit from and are harmed by trade coordination mechanisms.
constraint_indexing:constraint_classification(spirits_market_access_india, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The institutional apparatus of state spirits licensing (monopoly shops, license issuance, regulatory oversight) is substantially performative. Regulatory categories are opaque, administrative procedures are deliberately slow, and the ostensible purpose (social control, revenue collection) persists while actual regulatory function (quality control, safety verification) is minimal. The licensing theater persists through inertia despite widespread informal spirits markets (country liquor, illicit distillation) that completely bypass the formal regulatory apparatus. Theater ratio reflects that compliance theater is maintained for revenue and control narrative purposes despite low actual functional verification.
constraint_indexing:constraint_classification(spirits_market_access_india, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, state control over spirits markets can appear as an immutable expression of Indian cultural values (Gandhian sobriety), constitutional police powers, and public health authority. This perspective risks naturalizing a contingent institutional arrangement (licensing monopoly for revenue + protectionism) as a natural law of Indian governance. However, structural data reveals this as a false summit: the constraint's extraction and suppression are produced by institutional choices (monopoly licensing, discriminatory excise), not by inherent cultural or legal necessity.
constraint_indexing:constraint_classification(spirits_market_access_india, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spirits_market_access_india_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spirits_market_access_india, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spirits_market_access_india, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spirits_market_access_india, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spirits_market_access_india, TR),
    TR >= 0.70.

:- end_tests(spirits_market_access_india_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts monopoly rents from foreign importers (complete market exclusion, 150-200% tariff equivalent) and consumer surplus from domestic consumers (restricted choice, elevated prices through state markup). However, extractiveness is not maximal (0.72+) because: (a) informal markets capture significant consumer demand, reducing formal monopoly power, (b) state governments share extraction rents with bureaucratic apparatus rather than concentrating them, and (c) domestic producers face their own licensing constraints (not pure beneficiaries). The trend from 0.42 to 0.58 reflects increasing rent-extraction as state governments escalate tariff rates and tighten licensing in response to WTO pressure (counter-liberalization). Suppression (0.65): Moderate-high. Multiple barriers prevent market entry: state-level licensing monopolies with opaque issuance criteria, prohibitive excise taxation, regulatory category ambiguity, slow administrative processing, lack of dispute mechanisms. But suppression is not absolute (0.90+) because: (a) informal markets provide complete supply-side exit (though with quality/legal risk), (b) consumer demand exists in non-prohibition states, and (c) technological barriers are low (spirits production is globally commodified). Theater ratio (0.48): Moderate. Regulatory licensing and compliance procedures are somewhat performative — the stated goal (social control, public health) persists while actual enforcement (quality standards, safety verification, age-gating) is minimal and widely violated through informal markets. But theater is not dominant (0.70+) because: (a) excise revenue collection is functionally real, and (b) licensing monopoly structure does maintain control of formal distribution channels (even if informal channels escape).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates wide perspectival disagreement. The importer sees a snare designed to prevent their entry. The state sees coordination. The incumbent producer sees beneficial protection. The culturally-aligned consumer sees identity-based abstinence (not extraction). The trade body sees protectionist violation. The licensing apparatus sees its own theater. The civilizational observer risks seeing natural law. The gap reveals that the constraint's function (monopoly rent extraction + cultural framing + revenue collection + producer protection) is NOT equally visible from all positions. Beneficiaries experience it as coordination or protection. Victims experience it as snare or suppression. The constraint works precisely because this perspectival fragmentation prevents unified opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim declarations and exit options. Foreign importers have zero derived benefit and trapped exit, producing maximum d (0.95+) and maximum experienced extraction chi. State governments are structural beneficiaries with arbitrage options (could liberalize but choose not to for revenue), producing low d (0.10-0.20) and negative chi (they extract more than they bear). Domestic producers benefit from protection but face constrained exit (abandoning regulatory protection means international competition), producing moderate d (0.45-0.55). Prohibition-aligned consumers are structurally mobile (can access informal channels or travel to non-prohibition states) but identity-locked, producing high d (0.85-0.90) despite structural mobility — they cannot psychologically exercise their exit options. International trade bodies are organized challengers with constrained exit from multilateral commitments, producing moderate-high d (0.60-0.70). The piton classification does not derive from high chi (because theater_ratio is moderate, not dominant) but from the observation that licensing apparatus itself is performative while real market activity occurs outside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: it coordinates excise revenue collection and supply chain management (rope function) while simultaneously extracting monopoly rents and excluding foreign competitors (snare function). The constraint cannot be classified as pure coordination because of the asymmetric extraction; it cannot be classified as pure extraction because genuine coordination of tax collection occurs. The snare perspectives (importer, consumer demand) are real structural readings of exclusion. The rope perspective (state government) is a real structural reading of coordination. The scaffold perspective would emerge if liberalization timelines become credible (WTO deadline enforcement). The piton perspective reflects that the formal regulatory apparatus is largely theater while informal markets provide the actual supply function. The mandatrophy confirms that this is NOT a case of mislabeled coordination-as-extraction or vice versa — it is genuinely both. The resolution requires declaring all perspectives simultaneously true from their structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_protectionist_motivation,
    'Is the spirits market constraint primarily driven by genuine temperance/public health values or by state revenue maximization and producer protection?',
    'Analysis of excise revenue trends, license allocation patterns, state budget dependency on spirits revenue, and comparative enforcement of import restrictions vs domestic informal markets',
    'If primarily cultural: constraint may have lower extractiveness and higher legitimacy. If primarily protectionist: constraint is pure extraction justified through cultural framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_protectionist_motivation, empirical, 'Cultural values vs protectionist revenue motivation').

omega_variable(
    identity_lock_scope_and_durability,
    'What proportion of spirits market suppression is structural (legal barriers) vs identity-locked (internalized cultural identity preventing demand expression)?',
    'Regional comparison of spirits consumption patterns in prohibition vs non-prohibition states; consumer surveys on willingness-to-pay vs internalized abstinence norms; post-liberalization consumption elasticity',
    'If mostly structural barriers: suppress via removing legal restrictions. If mostly identity-locked: removing legal barriers alone will not increase consumption; requires cultural frame shift or generational change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_scope_and_durability, empirical, 'Proportion of suppression that is identity-locked vs structural').

omega_variable(
    informal_market_displacement,
    'Does the formal licensing monopoly actually control spirits distribution or is it displaced by country liquor, illicit distillation, and smuggling?',
    'Volume comparison of formal vs informal spirits consumption; seizure data on illicit distillation raids; cross-border smuggling volume estimates',
    'If formal system controls >80% of market: monopoly is functionally effective (coordination + extraction). If formal system controls <50%: monopoly is performative theater, extraction occurs via licensing rents while actual regulation is absent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_market_displacement, empirical, 'Whether formal licensing monopoly controls actual spirits distribution').

omega_variable(
    wto_compliance_pressure_trajectory,
    'Will international trade pressure (WTO dispute panel rulings, retaliatory tariffs) force India to liberalize spirits market access, and if so, what is the timeline?',
    'Tracking of WTO dispute status; trade negotiation progress; government statements on market liberalization; tariff retaliation timelines',
    'If liberalization occurs within 5 years: scaffold sunset logic applies (constraint is temporary institutional arrangement being replaced). If sustained >10 years: constraint has deeper structural roots than trade compliance alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wto_compliance_pressure_trajectory, empirical, 'WTO compliance pressure and liberalization trajectory').

omega_variable(
    domestic_producer_import_exposure,
    'Would domestic spirits producers actually be uncompetitive under open market conditions, or are they protected because of political economy rather than genuine competitive disadvantage?',
    'Comparative cost analysis of Indian producers vs imported spirits (manufacturing cost + transport + tariff); consumer preference testing in liberalized contexts; market share analysis if regional restrictions are temporarily lifted',
    'If genuinely uncompetitive: protectionism is temporary support during development (scaffold logic). If actually competitive: protectionism is pure extraction masking as market protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_producer_import_exposure, empirical, 'Whether domestic producers are genuinely uncompetitive or protected for political economy reasons').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spirits_market_access_india, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spirits_tr_t0, spirits_market_access_india, theater_ratio, 0, 0.38).
narrative_ontology:measurement(spirits_tr_t10, spirits_market_access_india, theater_ratio, 10, 0.43).
narrative_ontology:measurement(spirits_tr_t20, spirits_market_access_india, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(spirits_be_t0, spirits_market_access_india, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spirits_be_t10, spirits_market_access_india, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(spirits_be_t20, spirits_market_access_india, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spirits_market_access_india, resource_allocation).
narrative_ontology:affects_constraint(spirits_market_access_india, indian_alcohol_excise_revenue_dependency).
narrative_ontology:affects_constraint(spirits_market_access_india, country_liquor_informal_market_expansion).

% DUAL FORMULATION NOTE:
% Spirits market access constraint is downstream of state excise revenue structures. Upstream constraint (excise_revenue_dependency) has ε ≈ 0.35 (rope: genuine coordination of tax collection). Downstream constraint (country_liquor_expansion) has ε ≈ 0.72 (snare: informal market excludes regulation entirely). Current constraint (spirits_market_access) sits between them at ε ≈ 0.58, showing how upstream revenue logic justifies extraction that drives downstream informal market growth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spirits_market_access_india, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
