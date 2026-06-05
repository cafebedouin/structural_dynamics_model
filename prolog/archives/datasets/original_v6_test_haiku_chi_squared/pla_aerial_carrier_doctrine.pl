% ============================================================================
% CONSTRAINT STORY: pla_aerial_carrier_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pla_aerial_carrier_doctrine, []).

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
 *   constraint_id: pla_aerial_carrier_doctrine
 *   human_readable: China's Development of an Aerial Drone Carrier Doctrine
 *   domain: technological/political/military
 *
 * SUMMARY:
 *   China's development of an unmanned aerial vehicle (UAV) carrier platform
 *   — a large aerial vehicle capable of launching and recovering multiple
 *   smaller drones — represents a significant shift in regional air-sea power
 *   dynamics. This doctrine enables persistent aerial surveillance and
 *   interception over contested maritime territories without requiring
 *   carrier battle groups or land bases within contested zones. The
 *   constraint models the extraction mechanism created by this capability:
 *   unilateral air superiority that bypasses multilateral dispute resolution
 *   frameworks and prevents smaller regional nations from effectively
 *   contesting maritime claims. The doctrine operates in a gray zone between
 *   peacetime surveillance (lawful under international maritime law) and
 *   active blockade (which would constitute an act of war). This ambiguity is
 *   itself extractive: it allows the PLA to establish control over disputed
 *   waters without triggering unified allied military response.
 *
 * KEY AGENTS:
 *   - PLA Strategic Command: Primary beneficiary (institutional/arbitrage) — gains force projection, demonstrates technological parity, establishes control over regional air-sea commons
 *   - Contested Maritime Territories: Primary victim (powerless/trapped) — cannot exit; lacks countervailing air defense to contest surveillance or enforce sovereignty
 *   - Smaller Regional Nations: Secondary victim (moderate/constrained) — face escalating air superiority; constrained by budget and doctrine asymmetry; cannot develop equivalent capability
 *   - Allied Air Powers (US Navy, Regional Partners): Secondary actor (institutional/arbitrage) — have technical exits but are politically constrained; risk of global escalation limits escalation options
 *   - International Naval Convention System (UNCLOS): Organized system (organized/constrained) — coordinating principle (freedom of navigation) violated by unilateral air control; enforcement mechanism weakened
 *   - Analytical Observer: Global observer (analytical/analytical) — sees the doctrine as mixed coordination (legitimate deterrence) and extraction (unilateral control override of multilateral norms)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, 0.58).
domain_priors:suppression_score(pla_aerial_carrier_doctrine, 0.72).
domain_priors:theater_ratio(pla_aerial_carrier_doctrine, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pla_aerial_carrier_doctrine, snare).
narrative_ontology:human_readable(pla_aerial_carrier_doctrine, "China's Development of an Aerial Drone Carrier Doctrine").
narrative_ontology:topic_domain(pla_aerial_carrier_doctrine, "technological/political/military").

domain_priors:requires_active_enforcement(pla_aerial_carrier_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pla_aerial_carrier_doctrine, pla_strategic_command).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, regional_air_space_sovereignty).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, contested_maritime_territories).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, smaller_neighbor_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTESTED MARITIME TERRITORY (SNARE) — Cannot exit the constraint; has no countervailing air defense. A maritime zone claimed or disputed (South China Sea, East China Sea) faces persistent aerial reconnaissance, interception, and blockade through aerial carrier doctrine. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.74.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALLER REGIONAL NATIONS (SNARE) — Face escalating air superiority over territorial waters and airspace. Constrained by limited air defense budgets and doctrine asymmetry. Cannot field equivalent aerial carriers. d≈0.85, f(d)≈1.16, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ALLIED AIR POWERS (SNARE) — Have technical exits (carrier battle groups, advanced SAM systems) but operate under strategic constraints: cannot escalate to direct kinetic conflict without risking global war. Doctrinal arbitrage exists but is politically constrained. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLA STRATEGIC COMMAND (ROPE) — Benefits from the doctrine through force projection capability, demonstrates technological parity with Western navies, and establishes control over regional air-sea commons. Sees the constraint as coordination of its own tactical advantage. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL NAVAL CONVENTION SYSTEM (TANGLED ROPE) — Organized system (UNCLOS, freedom of navigation norms) both benefits from and is violated by aerial carrier doctrine. Coordinating principle: international waters are open to all. Extraction mechanism: unilateral air control of disputed waters invalidates the norm's enforceability. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/civilizational view, this doctrine represents mixed extraction and coordination. Coordination function: enables regional powers to pursue their legitimate strategic interests (deterrence, maritime security). Extraction function: unilateral air control that bypasses multilateral dispute resolution. ε=0.58, suppression=0.72 support tangled rope classification. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_aerial_carrier_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pla_aerial_carrier_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine creates genuine asymmetry in air control over disputed territories. The PLA can surveil and interdict shipping without equivalent countermeasures available to smaller nations. However, ε is not maximal (0.66+) because the doctrine operates in a gray zone — it is not outright blockade, and it does not prevent all alternative transit routes or deny all access to contested waters. The extraction is real but not total. Measurement trajectory: 0.35→0.58 reflects gradual capability maturation and integration into operational doctrine over 10-year interval. Suppression (0.72): High. Significant barriers to resistance include: (1) technical gap (no regional power has aerial carrier equivalent or rapid air defense deployment capability), (2) strategic asymmetry (allied response risks nuclear escalation, creating deterrent against intervention), (3) jurisdictional ambiguity (gray zone between surveillance and blockade, making UN action difficult), (4) geographic advantage (doctrine is optimized for contested waters where PLA bases are nearby, allies are distant). Theater ratio (0.38): Low-moderate. The doctrine has real functional content — actual surveillance, actual interception capability, actual threat to shipping. Performance is not primarily theatrical. The theater component comes from peacetime signaling: publicized tests and demonstrations serve deterrent messaging and domestic legitimacy, but the underlying capability is material, not performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the gap between structural victims and strategic beneficiaries. The contested maritime territory and smaller nations see a snare — they cannot exit and cannot retaliate at equivalent scale. Allied powers see a snare too, but with higher d (constrained rather than trapped) because they have technical exits (superior anti-UAV systems, cyber interdiction, pre-positioned defenses) even if escalation costs are high. The PLA sees a rope — force coordination achieving their strategic objective. The international maritime convention system sees tangled rope: the doctrine both relies on and violates UNCLOS principles (uses international waters, but denies freedom of navigation through unilateral air control). The analytical observer sees tangled rope at global scale: the doctrine is extractive (unilateral control override) but also coordinates PLA legitimate strategic interests (deterrence, security, rise assertion). The perspectival gap between the PLA (rope/beneficiary) and regional nations (snare/victim) is the core structural feature.
 *
 * DIRECTIONALITY LOGIC:
 *   PLA Strategic Command: Beneficiary + arbitrage exit → d≈0.05, f(d)≈-0.12. The PLA experiences the constraint as a beneficial coordination mechanism — a way to project power and assert control. Net beneficiary. Contested maritime territories: Victim + trapped exit → d≈0.92, f(d)≈1.40. No way to exit the zone; surveillance and interception capability cannot be resisted. Maximum extraction. Smaller regional nations: Victim + constrained exit → d≈0.85, f(d)≈1.16. Can purchase air defense systems, join allied coalitions, but cannot deploy air carriers equivalent to PLA capability, and alliance escalation carries nuclear risk. High extraction with some constrained alternatives. Allied air powers: Victim + arbitrage exit → d≈0.55, f(d)≈0.75. Have technical countermeasures (carrier groups, SAM systems, cyber), but strategic constraints (war-escalation risk) limit their utility. Moderate extraction. International maritime system: Victim + constrained exit → d≈0.68, f(d)≈1.02. The UNCLOS framework is being violated but cannot easily enforce; alternative frameworks (regional agreements) are weak. Moderate-high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint avoids the snare/tangled rope misclassification risk by recognizing that the doctrine contains a genuine coordination component (PLA strategic deterrence, assertion of great-power status) alongside pure extraction (denying maritime access to smaller powers). The mandatrophy is resolved through perspective differentiation: (1) From the PLA's perspective, this is rope — coordination of force projection. (2) From the victims' perspective, this is snare — unilateral extraction. (3) From the analytical observer's perspective, this is tangled rope — the doctrine both solves a coordination problem (PLA capability integration) and creates an extraction mechanism (unilateral air control). The claimed_type=snare reflects the base structural reality: the doctrine's primary function is to extract control from smaller nations over disputed waters. The tangled rope and rope perspectives show that the extraction is not purely coercive — it is bundled with genuine coordination for its beneficiary (the PLA). Mandatrophy is marked resolved because the perspectival decomposition clarifies the structure: extraction and coordination coexist, and their relative weighting depends on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escalation_threshold_definition,
    'At what level of aerial surveillance/interception does the doctrine cross from deterrent signaling into active blockade or territorial annexation?',
    'Incident classification: tracking provocations, intercepts, and airspace violations over time; analysis of PLA command decisions and escalation messaging',
    'If threshold is high (persistent surveillance without shootdown): constraint remains snare but with lower χ through ambiguity. If threshold is low (any intercept = active blockade): constraint strengthens to pure extraction snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_threshold_definition, empirical, 'Escalation threshold from surveillance to blockade').

omega_variable(
    technical_sustainability,
    'Can continuous aerial carrier operations maintain drone availability and pilot rotation without unsustainable logistics or attrition?',
    'Technical analysis of drone lifespan, repair cycles, pilot fatigue limits, and supply chain capacity; comparison with Western UAV sustainment models',
    'If unsustainable: doctrine is performative (theater_ratio should be higher). If sustainable: doctrine represents genuine capability shift and extraction mechanism holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_sustainability, empirical, 'Logistical sustainability of aerial carrier doctrine').

omega_variable(
    united_states_military_response,
    'Will the US develop and deploy countervailing aerial carriers, escalate to direct kinetic conflict, or accept regional air asymmetry and shift deterrence to nuclear or cyber domains?',
    'US strategic doctrine statements, force structure planning, defense budget allocation, and strategic communication; signals from DoD and State Department',
    'Symmetry response: constraint weakens to tangled rope (mutual deterrence coordination). Asymmetric response (cyber/nuclear shift): constraint strengthens as conventional air control becomes uncontested. Conflict: constraint becomes a casualty of kinetic warfare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(united_states_military_response, empirical, 'US strategic response to aerial carrier capability').

omega_variable(
    international_legal_status,
    'Do aerial drone carriers in international waters constitute legal surveillance under UNCLOS freedom of navigation, or do they violate coastal state rights to air defense exclusion zones?',
    'International law interpretation by maritime law scholars and UN bodies; incident-specific rulings by regional dispute resolution mechanisms',
    'If legal under freedom of navigation: constraint is snare (weaker international pushback, higher χ). If violation: stronger legal-diplomatic countervailing pressure, constraint weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_legal_status, conceptual, 'International legal status of aerial carrier operations').

omega_variable(
    coalition_counter_doctrine,
    'Can allied air powers (US, Japan, India, Australia) develop a joint doctrine that neutralizes aerial carrier advantage through coordinated air defense, cyber interdiction, or pre-emptive strikes?',
    'Analysis of multilateral defense agreements (Quad, AUKUS, Japan-South Korea cooperation); war games and doctrine publications; defense spending coordination',
    'If coalition emerges: constraint shifts from snare to tangled rope (coordinated deterrence). If coalition fails: PLA doctrine consolidates and snare classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_counter_doctrine, empirical, 'Potential coalition counter-doctrine formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pla_aerial_carrier_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plaac_tr_t0, pla_aerial_carrier_doctrine, theater_ratio, 0, 0.25).
narrative_ontology:measurement(plaac_tr_t5, pla_aerial_carrier_doctrine, theater_ratio, 5, 0.32).
narrative_ontology:measurement(plaac_tr_t10, pla_aerial_carrier_doctrine, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(plaac_be_t0, pla_aerial_carrier_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plaac_be_t5, pla_aerial_carrier_doctrine, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(plaac_be_t10, pla_aerial_carrier_doctrine, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pla_aerial_carrier_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, south_china_sea_sovereignty).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, taiwan_strait_air_superiority).
narrative_ontology:affects_constraint(pla_aerial_carrier_doctrine, regional_air_defense_doctrine).

% DUAL FORMULATION NOTE:
% Aerial carrier doctrine is distinct from but influences specific territorial disputes (South China Sea, Taiwan Strait). The doctrine represents a capability constraint on multiple territorial claims. Each territorial dispute has its own story with different base properties; this story models the doctrinal constraint that enables extraction across multiple territorial contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pla_aerial_carrier_doctrine, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
