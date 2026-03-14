% ============================================================================
% CONSTRAINT STORY: us_middle_east_force_posture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_middle_east_force_posture, []).

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
 *   constraint_id: us_middle_east_force_posture
 *   human_readable: US Middle East Force Posture as Coordination-Extraction Hybrid
 *   domain: geopolitics/military/regional_stability
 *
 * SUMMARY:
 *   The US military presence in the Middle East functions simultaneously as a
 *   security coordination mechanism (deterring regional escalation,
 *   protecting sea lanes, counterbalancing adversarial powers) and as a
 *   hegemonic extraction system (subordinating regional state autonomy,
 *   cementing allied dependency, extracting resource rents through security
 *   premium, constraining alternative security architectures). This
 *   constraint exemplifies the tangled_rope classification: both functions
 *   are structurally real, neither is incidental, and their coexistence
 *   creates a perspectival chasm between observers at different power levels
 *   and exit options. The force posture has grown more extractive over the
 *   35-year interval (extractiveness rising from 0.38 to 0.58) as post-Cold
 *   War military expansion replaced deterrence function with hegemonic
 *   consolidation. Theater ratio has also increased (0.52 to 0.68),
 *   reflecting growing divergence between stated strategic objectives
 *   (counterterrorism, regional stability, protecting allies) and actual
 *   operational patterns (force projection, sanctions enforcement, support
 *   for allied authoritarian regimes). The constraint's suppression component
 *   (0.62) reflects both structural barriers to exit (geopolitical isolation,
 *   lack of alternative security providers until recently) and institutional
 *   mechanisms that obscure extraction (strategic ambiguity, rotating
 *   justifications for presence, classification of operational data).
 *
 * KEY AGENTS:
 *   - US Strategic Command: Primary beneficiary (institutional/arbitrage) — maintains global military reach, regional hegemony, and freedom of action; can arbitrage forces globally
 *   - Civilian Populations in Conflict Zones: Primary victim (powerless/trapped) — bears direct costs of military presence and conflict escalation; cannot exit or organize resistance
 *   - Gulf State Allies (Saudi Arabia, UAE, Israel): Secondary beneficiary/constrained agent (institutional/constrained) — benefit from security umbrella but subordinated to US strategy; cannot credibly exit without facing regional vulnerability
 *   - Regional State Governments (Iran, Iraq, Syria): Secondary victim/constrained agent (moderate/constrained) — perceive extraction through loss of autonomy and constraints on regional power; constrained by security vulnerability if they exit
 *   - Global Arms Control Institutions: Tertiary victim (institutional/mobile) — institutional commitment to non-proliferation undermined by force posture's proliferation incentives; theater persists despite degraded function
 *   - Emerging Multipolar Alternatives (China, Russia, other Gulf partners): Scaffold architect (organized/mobile) — building alternative security and infrastructure partnerships that provide exit options for currently trapped agents
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees full complexity of tangled_rope structure; risks naturalizing extraction as coordination if bounded to single-perspective analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_middle_east_force_posture, 0.58).
domain_priors:suppression_score(us_middle_east_force_posture, 0.62).
domain_priors:theater_ratio(us_middle_east_force_posture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_middle_east_force_posture, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_middle_east_force_posture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_middle_east_force_posture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_middle_east_force_posture, tangled_rope).
narrative_ontology:human_readable(us_middle_east_force_posture, "US Middle East Force Posture as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(us_middle_east_force_posture, "geopolitics/military/regional_stability").

domain_priors:requires_active_enforcement(us_middle_east_force_posture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_middle_east_force_posture, us_strategic_interests).
narrative_ontology:constraint_beneficiary(us_middle_east_force_posture, gulf_state_allies).
narrative_ontology:constraint_beneficiary(us_middle_east_force_posture, regional_oil_markets).
narrative_ontology:constraint_victim(us_middle_east_force_posture, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(us_middle_east_force_posture, regional_state_autonomy).
narrative_ontology:constraint_victim(us_middle_east_force_posture, global_arms_control_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped by military presence and conflict dynamics with no exit option; bear direct costs (casualties, displacement, economic disruption) while experiencing maximum suppression through conventional and asymmetric warfare. Cannot organize collective exit or resistance; zero degrees of freedom.
constraint_indexing:constraint_classification(us_middle_east_force_posture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL STATE GOVERNMENTS (TANGLED ROPE) — Face both genuine coordination benefits (deterrence against shared adversaries, security guarantees) and extraction costs (loss of autonomous decision-making, subordination to US strategic preferences, constraint on regional power projection). Constrained exit due to geopolitical isolation costs; cannot fully exit without facing security vulnerability or sanctions.
constraint_indexing:constraint_classification(us_middle_east_force_posture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US STRATEGIC COMMAND (ROPE) — Perceives force posture as pure coordination mechanism: deterrence of regional aggression, security assurance to allies, maintenance of international rules-based order, freedom of navigation. Primary beneficiary with arbitrage exit options (can redeploy forces globally); experiences extraction flow toward themselves.
constraint_indexing:constraint_classification(us_middle_east_force_posture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GULF STATE ALLIES (TANGLED ROPE) — Benefit from security umbrella (deterrence, counterbalance to Iran), defense technology transfer, and privileged access to US markets. But constrained by dependency on US military support, subordination to US regional strategy, and pressure from US on human rights/normalization. Exit constrained by lack of alternative security providers; cannot credibly deter Iran without US backing.
constraint_indexing:constraint_classification(us_middle_east_force_posture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ARMS CONTROL INSTITUTIONS (PITON) — The force posture theaters compliance with non-proliferation and regional stability goals while actually driving proliferation incentives (Gulf states seek nuclear/advanced conventional weapons; Iran accelerates nuclear program as response). Theater_ratio high: institutions maintain performative commitment to non-proliferation while the underlying structural constraint incentivizes the opposite. Degraded function maintained through institutional inertia.
constraint_indexing:constraint_classification(us_middle_east_force_posture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTIPOLAR ALTERNATIVES (SCAFFOLD) — China-Russia-Gulf cooperation on infrastructure (Belt and Road), energy markets, and military partnerships represent an alternative security architecture. Sunset logic applies: as alternative providers mature and regional states diversify security relationships, the US force posture's monopoly extraction mechanisms degrade. Exit options improving for constrained agents as alternatives emerge; effective extraction declining as arbitrage options increase. Theater persists but is increasingly supplemented by functional alternatives.
constraint_indexing:constraint_classification(us_middle_east_force_posture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (deterrence of regional conflict escalation, maintenance of sea lanes, counterweight to rising adversarial powers) combined with asymmetric extraction (subordination of regional autonomy, installation of allied regimes, extraction of resource rents through security premium). Both functions coexist structurally; neither is reducible to the other. Classification depends on time horizon: immediate/biographical favors tangled rope; civilizational approaches scaffold (alternatives emerging).
constraint_indexing:constraint_classification(us_middle_east_force_posture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_middle_east_force_posture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_middle_east_force_posture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_middle_east_force_posture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_middle_east_force_posture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_middle_east_force_posture, TR),
    TR >= 0.70.

:- end_tests(us_middle_east_force_posture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The force posture extracts through multiple mechanisms: resource rents (security premium on oil), subordination of regional decision-making (allied regimes serve US interests), constraint on regional power competition (prevents Iran/Iraq/Syria from regional hegemony but installs allied hegemony instead), and dependency creation (allied states lack independent military capacity). But extractiveness is not maximum because: (1) genuine coordination benefits exist (deterrence does reduce some conflicts), (2) some allied states have non-trivial exit options (increasingly, through China/Russia alternatives), and (3) extraction is not purely coercive — many regional actors prefer US presence to alternative hegemons. Suppression (0.62): Moderate-high. Structural barriers to exit include lack of alternative security providers (until recently), economic dependency, geopolitical isolation, and conventional military asymmetries. Institutional suppression mechanisms include strategic ambiguity (unclear commitment conditions), classification of operational data (civilian casualty rates), and rotation of justifications (shifting threat narratives). Theater ratio (0.68): High. The stated rationale for presence (counterterrorism, regional stability, protecting allies) increasingly diverges from operational patterns (force projection capacity, support for authoritarian regimes, constraint on peer competition). Theater has increased over the interval as post-Cold War expansion added hegemonic consolidation functions that required more elaborate justification narratives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals the constraint's fundamental duality. US Strategic Command perceives Rope: the constraint coordinates deterrence and alliance maintenance with minimal perceived extraction overhead — they experience only coordination benefits. Gulf allies perceive Tangled Rope: genuine security benefits coexist with constrained autonomy; extraction is real but mixed with benefit. Regional adversaries perceive snare-moving-to-tangled-rope: the force posture primarily constrains their options, but with growing alternatives from multipolar coalition, exit becomes less impossible. Civilian populations perceive pure Snare: they experience only costs (casualties, displacement, economic disruption) with zero benefit and no exit. Analytical observer perceives the full structure: both coordination and extraction are real, neither reduces to the other, classification depends entirely on structural position. This gap demonstrates why single-perspective analysis fails for hegemonic constraints — the beneficiary's coordination narrative and the victim's extraction narrative are both empirically accurate descriptions of different layers of the same structural phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values map power levels and exit options to relative position within the extraction flow. US Strategic Command (institutional/arbitrage) has d ≈ 0.05 — full beneficiary status, high mobility, experiences negative effective extraction (extraction flows toward them). Gulf allies (institutional/constrained) have d ≈ 0.40 — mixed position, receive some benefits but constrained exit; moderate extraction. Regional state adversaries (moderate/constrained) have d ≈ 0.65 — more victim than beneficiary, but organized enough to partially resist; higher extraction. Civilian populations (powerless/trapped) have d ≈ 0.95 — full target status, maximum extraction. The multipolar coalition (organized/mobile) has d ≈ 0.35 — organized agents with improving exit options reduce experienced extraction as alternative providers mature. Engine applies sigmoid f(d) to each context, producing varying χ values that reflect this directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   HEGEMONIC CONSTRAINT PATTERN: The force posture resolves the mandatrophy by showing that 'coordination vs. extraction' is not a binary classification question but a structural fact with genuinely different manifestations across power levels. The US beneficiary sees coordination (Rope) — their structural position generates coordination benefits. The constrained ally sees tangled rope — their position is mixed. The trapped civilian sees snare — their position generates pure extraction. The analytical observer sees that all three perspectives are correct descriptions of the same structural phenomenon at different power levels. The mandatrophy is NOT resolved by choosing one type; it is resolved by recognizing that hegemonic constraints systematically appear as lower-extraction types to those at the top of the extraction hierarchy and as higher-extraction types (snare, tangled rope) to those bearing the costs. The misleading 'natural law' perspective emerges when the analyst privileges the beneficiary's viewpoint (US global hegemony is required for stability) without accounting for the victim's simultaneously accurate structural description (suppression of regional autonomy). Mandatrophy resolution requires multi-position analysis; no single index classifies the constraint completely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_hegemony_boundary,
    'Where is the boundary between legitimate collective security coordination and hegemonic extraction?',
    'Comparative analysis of alliance structures: NATO, QUAD, Gulf cooperation frameworks. Measure: do members perceive autonomous benefit from coordination, or only coerced compliance? Behavioral indicator: defection rates when enforcement pressure relaxes; degree of free-riding tolerated; institutional mechanisms for member voice in strategy.',
    'If boundary at high autonomy: most force posture is coordination (Rope from more perspectives). If boundary at minimal autonomy: force posture classifies as primarily extractive (Snare/Tangled Rope). Determines whether suppression metric reflects coercion or legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_hegemony_boundary, conceptual, 'Boundary between coordination and hegemonic extraction').

omega_variable(
    alternative_provider_credibility,
    'How credible are emerging alternatives (China military partnerships, Russia security guarantees) as genuine security providers vs. alternatives that carry their own extraction costs?',
    'Track defection/renegotiation rates among states using alternative providers; measure military capability gaps; analyze compliance with alliance obligations; assess long-term cost structures vs. US umbrella; model stability of alternative arrangements under stress (conflict, sanctions, default).',
    'If alternatives are durable: scaffold perspective confirmed; sunset timeline realistic; current suppression metrics overstate extraction (agents have real exit options). If alternatives collapse under stress: current allies remain trapped; suppression reflects structural asymmetry; scaffold is aspirational only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_provider_credibility, empirical, 'Whether emerging alternatives provide credible security').

omega_variable(
    civilian_harm_causation_attribution,
    'Are civilian harms in conflict zones attributable to the force posture itself, or to conflict dynamics that would exist regardless of US presence?',
    'Counterfactual analysis: compare civilian casualty rates, displacement, infrastructure damage in regions with vs. without US force presence; control for conflict intensity, state capacity, adversary capabilities. Analyze whether US presence reduces total conflict (deterrence effect) or concentrates it (flashpoint creation).',
    'If presence reduces total harm: suppression metric overstates extraction; civilian victims perspective shifts toward moderate agent experiencing mixed extraction/benefit. If presence increases harm: snare classification confirmed; suppression metrics accurate; victim group expands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_harm_causation_attribution, empirical, 'Whether force posture increases or reduces civilian harm').

omega_variable(
    theater_compliance_mechanism,
    'Is the high theater_ratio (0.68) driven by genuine institutional complexity or by intentional obscuration of extraction mechanisms?',
    'Linguistic analysis of US strategic documents vs. peer-reviewed security analysis: measure divergence in threat characterization. Assess public vs. classified threat assessments. Track alignment between stated military objectives and actual operational patterns. Measure: rate of force posture persistence after stated threats decline.',
    'If obscuration dominant: theater is a cover story; snare/tangled rope classification strengthened; extractiveness metric should rise. If genuine complexity: theater reflects real coordination costs; tangled rope classification confirmed as accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_compliance_mechanism, empirical, 'Whether theater ratio reflects complexity or intentional obscuration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_middle_east_force_posture, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmef_tr_t0, us_middle_east_force_posture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(usmef_tr_t20, us_middle_east_force_posture, theater_ratio, 20, 0.62).
narrative_ontology:measurement(usmef_tr_t35, us_middle_east_force_posture, theater_ratio, 35, 0.68).

% Extraction over time
narrative_ontology:measurement(usmef_be_t0, us_middle_east_force_posture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usmef_be_t20, us_middle_east_force_posture, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(usmef_be_t35, us_middle_east_force_posture, base_extractiveness, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_middle_east_force_posture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_middle_east_force_posture, 0.18).
narrative_ontology:affects_constraint(us_middle_east_force_posture, gulf_state_security_dependency).
narrative_ontology:affects_constraint(us_middle_east_force_posture, iranain_regional_isolation).
narrative_ontology:affects_constraint(us_middle_east_force_posture, global_arms_proliferation_incentives).
narrative_ontology:affects_constraint(us_middle_east_force_posture, us_military_hegemony_maintenance).

% DUAL FORMULATION NOTE:
% The force posture can be decomposed into structurally distinct constraints: (1) sea lane protection coordination (ε ≈ 0.15, Rope), (2) regional deterrence mechanism (ε ≈ 0.35, Tangled Rope with coordination dominant), (3) hegemonic consolidation extraction (ε ≈ 0.72, Snare with minimal coordination). This story combines all three under one constraint_id because they are operationally inseparable — the single deployed force simultaneously serves all three functions. Network links identify downstream constraints that depend on force posture maintenance for their structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_middle_east_force_posture, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
