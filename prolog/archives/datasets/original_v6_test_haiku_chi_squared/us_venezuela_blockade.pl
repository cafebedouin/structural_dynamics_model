% ============================================================================
% CONSTRAINT STORY: us_venezuela_blockade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_venezuela_blockade, []).

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
 *   constraint_id: us_venezuela_blockade
 *   human_readable: Proposed US Naval Blockade of Venezuelan Oil Tankers
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The proposed US naval blockade of Venezuelan oil tankers represents a
 *   geopolitical constraint combining energy market control, enforcement of
 *   hemispheric hegemony, and coercive economic statecraft. The constraint
 *   exhibits high extractiveness (0.68) and suppression (0.78) with
 *   relatively low theater (0.35), indicating that the coercive mechanism is
 *   direct and functional rather than performative. The blockade benefits a
 *   coalition of US strategic interests and competing oil producers while
 *   imposing severe costs on Venezuela, global energy consumers, and
 *   international maritime norms. Unlike many contemporary sanctions regimes,
 *   the blockade does not hide behind financial deception or secondary market
 *   mechanisms — it is a visible, kinetic demonstration of naval power
 *   projection. The theater ratio is low because the mechanism is structural
 *   enforcement, not ritual compliance. The blockade threatens international
 *   law norms but is justified by security exception clauses and demonstrated
 *   US naval capacity. The constraint's classification as Snare from the
 *   victim perspective is unambiguous: Venezuela cannot exit without state
 *   reorganization; the cost of blockade compliance is humanitarian collapse;
 *   suppression is enforced through naval presence and legal/insurance
 *   barriers to circumvention. From the beneficiary perspective (US strategic
 *   interests), the constraint exhibits Tangled Rope characteristics: it
 *   coordinates global energy markets and enforces dollar hegemony
 *   (coordination function) while simultaneously extracting market rents and
 *   political deference (extraction function). The constraint is downstream
 *   of broader energy geopolitics and upstream of humanitarian crisis and
 *   potential military escalation.
 *
 * KEY AGENTS:
 *   - Venezuelan Government and Oil Sector: Primary victim (powerless/trapped) — bears full cost of blockade through export collapse, humanitarian import constraint, and inability to exit sovereign territorial waters without military confrontation
 *   - Global Energy Consumers: Secondary victim (moderate/constrained) — face higher oil prices and constrained supply; cannot source Venezuelan oil due to US sanctions and insurance barriers
 *   - Caribbean and Latin American States: Secondary victim (moderate/constrained) — face pressure to enforce sanctions through territorial waters; constrained by geographic proximity to US power projection
 *   - Shipping Industry and Insurance Markets: Secondary victim (moderate/constrained) — cannot underwrite Venezuelan oil transport without US legal/financial penalties; exit options are constrained by liability exposure
 *   - US Strategic Establishment: Primary beneficiary (powerful/arbitrage) — captures geopolitical leverage, energy market control, and demonstration of hegemonic power; can exit from blockade commitment at low cost through policy change
 *   - OPEC and Competing Oil Producers: Beneficiary (organized/mobile) — benefit from supply reduction and higher prices; experienced blockers who can adapt to altered market conditions
 *   - International Maritime and Trade System: Institutional actor (institutional/constrained) — sustains performative commitment to rules-based order while experiencing functional degradation of legal authority; constrained by US enforcement capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses blockade as instantiation of hegemonic snare architecture constraining state energy sovereignty globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_blockade, 0.68).
domain_priors:suppression_score(us_venezuela_blockade, 0.78).
domain_priors:theater_ratio(us_venezuela_blockade, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_blockade, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_venezuela_blockade, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_venezuela_blockade, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_venezuela_blockade, snare).
narrative_ontology:human_readable(us_venezuela_blockade, "Proposed US Naval Blockade of Venezuelan Oil Tankers").
narrative_ontology:topic_domain(us_venezuela_blockade, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_venezuela_blockade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_venezuela_blockade, us_strategic_interests).
narrative_ontology:constraint_beneficiary(us_venezuela_blockade, competing_oil_producers).
narrative_ontology:constraint_victim(us_venezuela_blockade, venezuela_oil_sector).
narrative_ontology:constraint_victim(us_venezuela_blockade, global_energy_consumers).
narrative_ontology:constraint_victim(us_venezuela_blockade, shipping_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN OIL SECTOR (SNARE) — Cannot exit the constraint; oil export is the primary revenue source for state operations and humanitarian imports. Blockade forces choice between compliance (economic collapse) or attempted circumvention (military escalation). d≈0.96, f(d)≈1.41, σ=1.2 → χ≈0.76. Pure extraction with maximum coercion.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CARIBBEAN AND LATIN AMERICAN STATES (SNARE) — Constrained by geographic proximity to US power projection, economic dependence on US trade, and inability to counter naval blockade. Face pressure to enforce sanctions or restrict Venezuelan shipping through territorial waters. d≈0.82, f(d)≈1.18, σ=0.9 → χ≈0.63. High extraction with coercive pressure.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC ESTABLISHMENT (TANGLED ROPE) — Benefits from demonstrated power projection, energy market control, and geopolitical leverage. Blockade enforces coordination (dollar dominance in energy markets, US-led liberal order enforcement) but requires sustained military presence and faces international legal challenges. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.23. Net beneficiary with mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(us_venezuela_blockade, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL OIL PRODUCERS AND OPEC (ROPE) — Benefits from supply reduction that supports higher oil prices. Blockade solves collective action problem of keeping Venezuelan oil off global market without explicit cartel coordination. d≈0.20, f(d)≈0.06, σ=1.2 → χ≈0.05. Low effective extraction; primarily coordination benefit.
constraint_indexing:constraint_classification(us_venezuela_blockade, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL ENERGY CONSUMERS AND SHIPPING INDUSTRY (SNARE) — Face higher energy prices, constrained supply, and legal/insurance barriers to Venezuelan oil transport. Shipping companies cannot insure Venezuelan tankers without US compliance risk. d≈0.78, f(d)≈1.14, σ=1.2 → χ≈0.71. High extraction through coercive supply control.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME AND TRADE SYSTEM (TANGLED ROPE) — Experiences blockade as both coordination mechanism (US-enforced rules-based order) and extraction mechanism (precedent for unilateral naval enforcement erodes international law norms). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.62. Significant tension between stated coordination function (orderly markets) and actual extraction (coercive power projection).
constraint_indexing:constraint_classification(us_venezuela_blockade, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Blockade persists despite contradicting UN Convention on the Law of the Sea and international commerce norms. Framework maintains performative legitimacy (security exception clauses, US veto power in enforcement) but functional authority is degraded. theater_ratio≈0.35 does NOT satisfy piton gate (theater must be ≥0.70). Engine reclassifies as Tangled Rope: legal framework provides enough normative coordination function to justify active enforcement, but extraction via law-breaking is the actual mechanism.
constraint_indexing:constraint_classification(us_venezuela_blockade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From civilizational scale, blockade represents structural enforcement mechanism of US monetary/military hegemony over global energy markets. Extraction is systemic: any state seeking energy independence or non-dollar trade faces blockade threat. d≈0.74, f(d)≈1.13, σ=1.2 → χ≈0.73. This perspective reveals blockade as instantiation of larger snare architecture.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_blockade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_venezuela_blockade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_venezuela_blockade, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_venezuela_blockade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_venezuela_blockade, TR),
    TR >= 0.70.

:- end_tests(us_venezuela_blockade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The blockade extracts significant economic rents (higher global oil prices benefiting non-Venezuelan producers), geopolitical deference (other states enforce secondary sanctions), and market control (constrains global energy trading). The extraction grows over time as circumvention routes are closed and alternatives are foreclosed. Initial extractiveness (t=0) lower (0.42) due to transshipment and alternative route availability; as blockade hardens over 12 months, extraction increases (t=12: 0.68) as third-country enforcement becomes normalized. Suppression (0.78): Very high. Victims face multiple suppression mechanisms: (1) naval interdiction of tankers, (2) US financial sanctions on companies trading Venezuelan oil, (3) insurance market denial (Lloyd's, major brokers avoid Venezuelan business), (4) secondary sanctions on countries that circumvent, (5) legal barriers under US law. Escape routes require ship-to-ship transfers in international waters, alternative payment systems, and third-country complicity — all high-risk, high-cost operations. Theater ratio (0.35): Low. The blockade is not performative — it is kinetic enforcement. Unlike financial sanctions regimes that hide their extraction behind market mechanisms, the blockade is visible enforcement of power. The 35% theater reflects the minimal normative framing (security exceptions, counter-narcotics justification) relative to the direct coercive mechanism. Theater declines from t=0 (0.50) to t=12 (0.35) as the initial justification apparatus (counter-narcotics framing) becomes less credible and the pure power projection becomes undeniable.
 *
 * PERSPECTIVAL GAP:
 *   MAXIMAL PERSPECTIVAL DIVERGENCE. Venezuelan oil sector sees pure Snare: no exit, total extraction, maximum coercion. US strategic establishment sees Tangled Rope: coordination of energy markets plus extraction of rents. Global oil producers see Rope: coordination of supply reduction without explicit cartel. Caribbean states see Snare from their position (constrained, threatened by US enforcement). International maritime system sees Tangled Rope: coordination function (orderly energy markets) versus extraction function (precedent for unilateral naval enforcement). Shipping industry sees pure Snare: cannot operate Venezuelan routes. Energy consumers see Snare: constrained supply, higher prices. This perspectival gap reflects fundamental conflict between the beneficiary's ordering logic (hegemonic stability through US power projection) and the victim's structural reality (coercive exclusion from global energy markets). The gap is not epistemological — the structural relationships are objectively different. The beneficiary genuinely experiences coordination benefits; the victim genuinely experiences extraction with no alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Venezuelan oil sector: Victim + trapped → d≈0.96. Maximum directionality. No exit capacity; all adjustment burden on Venezuela. f(d)≈1.41, σ=1.2 → χ≈0.76. Highest effective extraction. Global energy consumers: Victim + constrained → d≈0.78. Can substitute other energy sources or accept higher prices, but with significant cost. f(d)≈1.14, σ=1.2 → χ≈0.71. High extraction. Caribbean states: Victim + constrained → d≈0.82. Can theoretically refuse to enforce sanctions, but face secondary sanctions threat. f(d)≈1.18, σ=0.9 → χ≈0.63. Extraction medium-high. US strategic establishment: Beneficiary + arbitrage → d≈0.35. Can terminate blockade at no cost; experiences constraint as voluntary coordination mechanism. f(d)≈0.28, σ=1.2 → χ≈0.23. Net beneficiary. OPEC producers: Beneficiary + mobile → d≈0.20. Can adjust production or exit cartel; benefits are automatic (price support). f(d)≈0.06, σ=1.2 → χ≈0.05. Minimal extraction. International maritime system: Institutional + constrained → d≈0.55. Cannot exit enforcement regime without challenging US naval dominance. f(d)≈0.75, σ=1.2 → χ≈0.62. Tangled Rope zone.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The blockade classifies as Snare at ε=0.68 with suppression=0.78 and χ≥0.66 from the victim perspective, satisfying all three gates for Snare classification. The mandatrophy (is this really extraction, or is it coordination?) is resolved by the perspectival method: from the beneficiary perspective (US), the constraint does exhibit coordination function (energy market stability, hegemonic order enforcement) — hence Tangled Rope from that view. From the victim perspective (Venezuela), there is zero coordination function — the constraint is pure extraction. The engine's perspectival gap measurement reveals the mandatrophy: the beneficiary's Tangled Rope and the victim's Snare are not different empirical observations of the same constraint, but rather different structural experiences of the same power relationship. The constraint IS a Snare if the victim's structural position is taken as primary (no exit, no coordination benefit); it IS a Tangled Rope if the beneficiary's justification (energy market coordination, rules-based order) is taken as primary. The resolution is to declare BOTH classifications as valid perspectival truths and measure the gap: a perspectival gap of 3+ types (Tangled Rope vs Snare) indicates a constraint whose legitimacy is fundamentally disputed. The mandatrophy is resolved not by choosing one type, but by recognizing that the constraint's function is radically different depending on structural position — which is itself a diagnosis of extraction without coordination consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_authority_ambiguity,
    'Does US have legitimate legal authority under international law to enforce unilateral blockade of Venezuelan oil exports, or is this a precedent-setting violation of UN Convention on Law of the Sea?',
    'International court opinions; UN General Assembly resolutions; state responses and countermeasures; enforcement of precedent in subsequent blockade scenarios',
    'If legitimate: blockade is Rope-leaning (coordination of rules-based order enforcement). If violation: blockade is pure Snare (coercive power without legal wrapper).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_authority_ambiguity, conceptual, 'Whether blockade has legitimate international legal authority').

omega_variable(
    sanction_effectiveness_threshold,
    'What degree of economic pressure on Venezuela constitutes ''coercion'' vs ''legitimate economic incentive''? At what poverty/mortality threshold does constraint shift from Snare to Tangled Rope?',
    'Humanitarian impact assessments; mortality data; comparative analysis with other blockades (Cuba, Iraq, North Korea); Venezuelan government response strategies',
    'If threshold high (near state collapse): constraint remains pure Snare from victim perspective throughout enforcement window. If threshold moderate: constraint may shift to Tangled Rope if humanitarian costs trigger coalition pressure for negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_effectiveness_threshold, preference, 'Threshold at which economic pressure becomes coercive extraction').

omega_variable(
    circumvention_route_persistence,
    'Can Venezuelan oil reach markets through alternative routes (ship-to-ship transfers, transshipment via third countries, alternative payment systems), thereby converting constrained exit to mobile exit?',
    'Empirical tracking of oil transport routes; sanctions evasion analysis; third-country compliance with secondary sanctions; alternative payment system adoption',
    'If routes remain open: victims shift from trapped to mobile exit, reclassifying from Snare to Tangled Rope from victim perspective. If blockade closes routes: Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circumvention_route_persistence, empirical, 'Whether circumvention routes allow Venezuelan oil to reach markets').

omega_variable(
    coalition_formation_velocity,
    'Do non-aligned states (China, India, Russia) form a counter-coalition to challenge blockade and establish alternative shipping/payment systems fast enough to create organized opposition before constraint hardens?',
    'Emergence of explicit counter-sanctions; alternative shipping protocols; BRICS+ energy trade agreements; sanctions-busting ship registries',
    'If coalition forms rapidly: constraint shifts to Tangled Rope for counter-coalition members (organized/mobile exit). If coalition slow: blockade remains pure Snare for all victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_velocity, empirical, 'Speed of counter-coalition formation to challenge blockade').

omega_variable(
    humanitarian_exception_triggers,
    'Does blockade include carve-outs for humanitarian imports (food, medicine, electricity generation), and if so, how binding are they in practice?',
    'Monitoring of humanitarian exemption requests and approval rates; death rates among vulnerable populations; comparison with other blockade humanitarian protocols',
    'If exemptions are robust: constraint is Tangled Rope (coordination function for emergency access plus extraction through rationing). If exemptions become dead-letter: constraint is pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_exception_triggers, empirical, 'Enforceability of humanitarian exceptions in blockade implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_venezuela_blockade, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usvb_tr_t0, us_venezuela_blockade, theater_ratio, 0, 0.5).
narrative_ontology:measurement(usvb_tr_t6, us_venezuela_blockade, theater_ratio, 6, 0.38).
narrative_ontology:measurement(usvb_tr_t12, us_venezuela_blockade, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(usvb_be_t0, us_venezuela_blockade, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usvb_be_t6, us_venezuela_blockade, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(usvb_be_t12, us_venezuela_blockade, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_venezuela_blockade, resource_allocation).
narrative_ontology:affects_constraint(us_venezuela_blockade, opec_production_coordination).
narrative_ontology:affects_constraint(us_venezuela_blockade, usd_energy_dominance).
narrative_ontology:affects_constraint(us_venezuela_blockade, international_maritime_law_erosion).

% DUAL FORMULATION NOTE:
% The blockade is downstream of broader US energy hegemony strategy (constraining alternative suppliers globally) and upstream of humanitarian crisis and military escalation risk. Decomposition: (1) Naval enforcement mechanism (constraint_id: us_venezuela_blockade, ε≈0.68, Snare) — the kinetic blockade itself. (2) Market extraction mechanism (constraint_id: usd_energy_dominance, ε≈0.52, Tangled Rope) — the broader system of using dollar dominance to control energy trading. The blockade is a concrete enforcement instance of the abstract extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_venezuela_blockade, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
