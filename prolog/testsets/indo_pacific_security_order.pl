% ============================================================================
% CONSTRAINT STORY: indo_pacific_security_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_pacific_security_order, []).

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
 *   constraint_id: indo_pacific_security_order
 *   human_readable: Indo-Pacific Security Order: Great Power Competition and Regional Stability
 *   domain: geopolitical/security/international_relations
 *
 * SUMMARY:
 *   The Indo-Pacific security order represents a hybrid constraint structure
 *   that simultaneously provides regional coordination (preventing conflicts,
 *   enabling commerce, maintaining alliance commitments) and extracts from
 *   smaller states through great power competition, technology control, and
 *   forced alignment. The constraint has intensified from 2015-2025 as
 *   China's capabilities grew, competition for strategic chokepoints
 *   sharpened, and alliance pressure increased. The extractiveness trajectory
 *   (0.35→0.58) reflects growing great power militarization and reduced
 *   optionality for mid-tier states. The theater ratio trajectory (0.42→0.68)
 *   reflects increasing performative activity: joint exercises, institutional
 *   meetings, and strategic signaling have grown while actual coordination
 *   mechanisms have ossified (Cold War alliance architecture persisting) or
 *   fractured (consensus-based ASEAN frameworks losing efficacy). This
 *   constraint demonstrates all six types from different perspectives,
 *   revealing how institutional arrangements naturalizing as 'security
 *   necessity' actually embed asymmetric extraction.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — maintains regional hegemony, access to critical technologies, alliance control
 *   - China: Challenger power (organized/constrained) — benefits from global integration but constrained by containment alliance structure
 *   - Small island states and developing economies: Primary victims (powerless/trapped) — caught in great power competition with no exit options
 *   - Mid-tier regional powers (India, Vietnam, Indonesia): Secondary victims (moderate/constrained) — leverage some negotiating power but face alliance pressure
 *   - Cold War alliance architecture: Institutional actor (institutional/arbitrage) — persists through inertia despite changed strategic context
 *   - ASEAN consensus frameworks: Regional coordinator (organized/mobile) — provides temporary stability and agency for smaller states with gradual sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_pacific_security_order, 0.58).
domain_priors:suppression_score(indo_pacific_security_order, 0.65).
domain_priors:theater_ratio(indo_pacific_security_order, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_pacific_security_order, extractiveness, 0.58).
narrative_ontology:constraint_metric(indo_pacific_security_order, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indo_pacific_security_order, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_pacific_security_order, tangled_rope).
narrative_ontology:human_readable(indo_pacific_security_order, "Indo-Pacific Security Order: Great Power Competition and Regional Stability").
narrative_ontology:topic_domain(indo_pacific_security_order, "geopolitical/security/international_relations").

domain_priors:requires_active_enforcement(indo_pacific_security_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_pacific_security_order, united_states_regional_hegemony).
narrative_ontology:constraint_beneficiary(indo_pacific_security_order, allied_maritime_access).
narrative_ontology:constraint_beneficiary(indo_pacific_security_order, technology_standard_setters).
narrative_ontology:constraint_victim(indo_pacific_security_order, smaller_regional_states).
narrative_ontology:constraint_victim(indo_pacific_security_order, global_south_development).
narrative_ontology:constraint_victim(indo_pacific_security_order, nuclear_proliferation_risks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATES (SNARE) — Caught in great power competition over their territory and waters. Limited economic alternatives, climate vulnerability, and strategic location create structural entrapment. These states experience extraction through resource control, debt diplomacy, and forced alignment choices with minimal genuine agency. No credible exit option within their lifetime horizon.
constraint_indexing:constraint_classification(indo_pacific_security_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-TIER REGIONAL POWERS (TANGLED ROPE) — India, Vietnam, Indonesia, and others benefit from security coordination against regional instability and from access to great power competition for leverage. Yet they experience extraction through alliance pressure, forced technology choices, and sovereignty constraints. Constrained by geopolitical dependencies but retain negotiating leverage and some exit capacity via non-alignment or pivot tactics.
constraint_indexing:constraint_classification(indo_pacific_security_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — Experiences the security order as coordination mechanism: maintaining alliances, ensuring freedom of navigation, preventing regional conflicts that would disrupt great power competition. The US can arbitrage between alliance options, technology standards, and security frameworks. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(indo_pacific_security_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINA (TANGLED ROPE) — Simultaneously benefits from and is constrained by the existing security order. Benefits from global maritime commerce and economic integration; constrained by alliance networks limiting its regional freedom of action and technology access. Organized power with genuine alternatives (Belt and Road, regional balancing) but experiences extraction through containment logic and technology restrictions.
constraint_indexing:constraint_classification(indo_pacific_security_order, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR ALLIANCE ARCHITECTURE (PITON) — NATO, hub-and-spoke alliances, and bilateral defense treaties persist through institutional inertia despite changed strategic context. Theater ratio (0.68) reflects that much of the visible security activity—joint exercises, formal meetings, institutional maintenance—is performative continuation of post-WWII frameworks. Primary function (containing Soviet expansion) has atrophied; constraint persists through bureaucratic and political inertia.
constraint_indexing:constraint_classification(indo_pacific_security_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ASEAN CONSENSUS FRAMEWORKS (SCAFFOLD) — ASEAN Regional Forum, East Asia Summit, and consensus-based diplomatic norms create a temporary coordination mechanism with gradual sunset logic. These frameworks provide regional stability and agency for smaller states during the great power transition. The sunset emerges from structural change: as China's capabilities grow and great power competition intensifies, consensus-based frameworks lose their buffering function. Estimated sunset: 10-20 years as the balance-of-power framework becomes more rigid.
constraint_indexing:constraint_classification(indo_pacific_security_order, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From a civilizational scope, the Indo-Pacific security order reflects immutable geographic and demographic facts: chokepoint straits, trade dependency, nuclear weapons, and power imbalance create structural constraints that no actor can escape. This perspective risks naturalizing contingent institutional choices (alliance formation, containment strategy) as inevitable. The engine will identify this as a false summit—the 'inevitability' is institutional, not natural.
constraint_indexing:constraint_classification(indo_pacific_security_order, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_pacific_security_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_pacific_security_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_pacific_security_order, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_pacific_security_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_pacific_security_order, TR),
    TR >= 0.70.

:- end_tests(indo_pacific_security_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The security order has shifted toward great power competition, increasing resource demands on allies and smaller states. Technology control, alliance contributions, and military buildups concentrate costs on less powerful states. Theater ratio (0.68): High and rising. Performative security activity—joint exercises, formal meetings, signaling—has increased faster than actual coordination mechanisms. Cold War alliance architecture persists through inertia; ASEAN consensus frameworks are losing functional role. Suppression (0.65): Moderate-high. Small states face real barriers to exit: geographic vulnerability, economic dependency, security threats from other actors, and alliance pressure. But suppression is not total—some states (Thailand, Vietnam) maintain flexible alignments; others (Philippines) oscillate between great powers. The measurement trajectory shows extraction accelerating from 2015-2025 as competition intensified.
 *
 * PERSPECTIVAL GAP:
 *   The United States sees primarily coordination benefits (Rope): maintaining alliances, ensuring FON, preventing regional conflicts that disrupt great power competition. China sees mixed constraints and opportunities (Tangled Rope): benefits from global commerce and some regional influence; constrained by alliance containment and technology restrictions. Small states see entrapment (Snare): caught between great powers with limited genuine choices. Mid-tier powers see mixed extraction and leverage (Tangled Rope): they can negotiate between great powers but face pressure to align. The Cold War alliance architecture appears to its operators as functional (they maintain its institutions), but the wider perspective reveals it as Piton—performing functions it no longer serves. ASEAN consensus frameworks appear as temporary solutions (Scaffold) with a sunset as great power competition intensifies and consensus becomes impossible. The analytical observer's 'inevitability' view is a false summit: the specific alliance structures, technology controls, and competition framings are institutional choices, not natural laws of geography.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to extraction flow. The United States as beneficiary with arbitrage exit options derives low d (~0.15), producing negative effective extraction χ despite moderate base ε. Small states as victims with trapped exit derive high d (~0.92), producing high χ despite moderate base ε. Mid-tier powers as victims with constrained exit derive moderate-high d (~0.72), producing moderate extraction. China as both competitor and beneficiary derives mid-range d (~0.58) reflecting mixed position—benefits from global integration but constrained by containment. The Cold War alliance structure as institutional actor with arbitrage options derives low d (~0.12). ASEAN frameworks as organized actors with some mobile options derive moderate d (~0.45). The perspectival gap emerges from differentiated directionality: the same base extractiveness of 0.58 produces high χ for small states, moderate χ for mid-tier powers, low χ for the US and its allies.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how 'security coordination' serves as a cover story for great power extraction. The Rope classification (US perspective) describes genuine coordination functions: preventing regional conflicts, maintaining commerce security, ensuring alliance credibility. But this coordination is asymmetrically distributed—it benefits the hegemonic power more than the constrained allies. The Snare classification (small state perspective) describes the ground reality: trapped agents with no exit who bear the costs of great power competition. The Tangled Rope classifications (mid-tier and Chinese perspectives) describe the actual structure: these agents benefit from some coordination functions while experiencing extraction through constraint. The Piton classification (Cold War alliance architecture) reveals institutional inertia—the frameworks persist through bureaucratic momentum, not because they solve the problems they were designed for. The Scaffold classification (ASEAN consensus) identifies the genuine sunset mechanism: as great power competition intensifies and military capabilities diverge, consensus-based frameworks lose their buffering function. The false summit (analytical inevitability) shows how geographic and demographic facts get naturalized into institutional choices—the straits are geography; the alliance system is choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    great_power_war_probability,
    'Does the security order structure increase or decrease the probability of direct great power military conflict?',
    'Historical pattern analysis of great power competition under different security architectures; crisis incident tracking and escalation dynamics; game-theoretic modeling of alliance credibility',
    'If probability increases: classify toward Snare (extraction through externalized risk). If probability decreases: classify toward Rope (genuine coordination benefit). If ambiguous: Tangled Rope confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_war_probability, empirical, 'Whether security order increases or decreases great power conflict risk').

omega_variable(
    small_state_development_cost,
    'What portion of small island and regional state development constraints is attributable to the security order versus other structural factors (climate, geography, colonial legacy)?',
    'Counterfactual economic modeling: development trajectory with and without security competition; analysis of resource allocation patterns; comparison to non-aligned states',
    'If security order accounts for >40% of constraint: victim extraction confirmed. If <20%: reframe as secondary to other constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_development_cost, empirical, 'Proportion of development constraints attributable to security order').

omega_variable(
    technology_standard_lock_in,
    'Are US-aligned technology standards and export controls producing lock-in costs for mid-tier and developing states, or are they substitutable through alternative (Chinese, Indian) standards?',
    'Technology adoption patterns; cost analysis of standard switching; trajectory of non-aligned technology ecosystems; supply chain dependencies',
    'If lock-in is real: extract mechanism confirmed. If substitutable: extraction narrative is weaker, reframe as coordination with options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_standard_lock_in, empirical, 'Whether technology standards create extractive lock-in').

omega_variable(
    alliance_credibility_equilibrium,
    'Do alliance commitments (US defense guarantees, regional security partnerships) represent genuine coordination equilibria or are they contingent on hegemonic power maintenance?',
    'Historical analysis of alliance durability under power shifts; signaling and commitment mechanisms; allied state exit capacity; comparison to multipolar alliance structures',
    'If genuine equilibrium: Rope classification more justified. If contingent on hegemony: Snare (for allies trapped by dependence) and Tangled Rope (for challengers) better fit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_credibility_equilibrium, conceptual, 'Whether alliances are stable equilibria or hegemony-dependent').

omega_variable(
    free_shipping_versus_extraction_tradeoff,
    'Does freedom of navigation enforcement provide net public goods (open sea lanes, commerce security) or is it primarily a mechanism for enforcing great power resource access?',
    'Cost-benefit analysis for maritime trade; comparison of shipping safety and costs under different enforcement regimes; historical analysis of FON enforcement patterns; shipping insurance data',
    'If public goods dominate: Rope classification justified. If extraction dominates: Snare (for states dependent on shipping) confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_shipping_versus_extraction_tradeoff, empirical, 'Whether freedom of navigation provides public goods or enforces resource extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_pacific_security_order, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indo_tr_t0, indo_pacific_security_order, theater_ratio, 0, 0.42).
narrative_ontology:measurement(indo_tr_t5, indo_pacific_security_order, theater_ratio, 5, 0.55).
narrative_ontology:measurement(indo_tr_t10, indo_pacific_security_order, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(indo_be_t0, indo_pacific_security_order, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indo_be_t5, indo_pacific_security_order, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(indo_be_t10, indo_pacific_security_order, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_pacific_security_order, enforcement_mechanism).
narrative_ontology:affects_constraint(indo_pacific_security_order, us_china_technology_competition).
narrative_ontology:affects_constraint(indo_pacific_security_order, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(indo_pacific_security_order, quad_alliance_coordination).
narrative_ontology:affects_constraint(indo_pacific_security_order, semiconductor_supply_chain_security).

% DUAL FORMULATION NOTE:
% The Indo-Pacific security order decomposes into specific constraints (FON enforcement, technology standards, alliance coordination) each with distinct ε values. This story captures the overarching coordination-extraction hybrid; downstream constraints inherit its structural properties but have their own extractiveness metrics reflecting specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_pacific_security_order, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
