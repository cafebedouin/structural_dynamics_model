% ============================================================================
% CONSTRAINT STORY: condiment_tyranny
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_condiment_tyranny, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: condiment_tyranny
 *   human_readable: The Tyranny of the Default Condiment Offering
 *   domain: social/economic/food_service
 *
 * SUMMARY:
 *   The condiment tyranny emerges from the convergence of supply chain
 *   economics, consumer habit formation, and institutional standardization in
 *   large-scale food service. Fast-food chains, casual dining operators, and
 *   food delivery platforms have created a default condiment ecology —
 *   ketchup, mustard, mayonnaise, occasionally hot sauce — that has become so
 *   normalized that it appears inevitable rather than contingent. However,
 *   this standardization is enforced through economic gatekeeping: niche
 *   condiment producers face insurmountable barriers to shelf space and table
 *   presence in major food service channels. The constraint exhibits a
 *   genuine coordination function (standardization does reduce costs and
 *   simplify logistics) alongside asymmetric extraction (large chains and
 *   default manufacturers capture the benefits while diversity of supply is
 *   suppressed). The rising theater ratio reflects that the ritual of
 *   offering three condiment packets has become increasingly performative as
 *   online platforms (DoorDash, Uber Eats) reduce the logistical constraints
 *   that once made standardization necessary. Alternative dining ecosystems
 *   (food trucks, ethnic restaurants, premium casual chains, farmers markets)
 *   are eroding the dominance of the default standard by demonstrating that
 *   consumers will pay modest premiums for condiment diversity. The
 *   constraint appears to be degrading from a necessary coordination
 *   mechanism into an inertial piton — maintained through habit and switching
 *   costs rather than functional necessity.
 *
 * KEY AGENTS:
 *   - Large Food Service Chains: Primary beneficiary (institutional/arbitrage) — capture economies of scale and operational simplicity through standardization
 *   - Default Condiment Manufacturers: Secondary beneficiary (institutional/arbitrage) — guaranteed shelf presence and volume contracts ensure market dominance
 *   - Niche Condiment Producers: Primary victim (powerless/trapped) — face distribution gatekeeping and cannot access large-scale food service markets without accepting standardized offerings
 *   - Individual Restaurant Operators: Secondary victim/constrained beneficiary (moderate/constrained) — benefit from cost reduction but constrained by customer expectations and supplier lock-in
 *   - Supply Chain Logistics: Institutional beneficiary (institutional/arbitrage) — standardization reduces complexity and inventory variance across distributed networks
 *   - Consumer Choice Diversity: Abstract victim (powerless/trapped) — dietary, cultural, and preference pluralism is suppressed through limited default options
 *   - Alternative Dining Ecosystem: Organized agent (organized/mobile) — farmers markets, food trucks, ethnic restaurants, premium casual chains gradually erode default standardization through viable alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(condiment_tyranny, 0.38).
domain_priors:suppression_score(condiment_tyranny, 0.52).
domain_priors:theater_ratio(condiment_tyranny, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(condiment_tyranny, extractiveness, 0.38).
narrative_ontology:constraint_metric(condiment_tyranny, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(condiment_tyranny, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(condiment_tyranny, tangled_rope).
narrative_ontology:human_readable(condiment_tyranny, "The Tyranny of the Default Condiment Offering").
narrative_ontology:topic_domain(condiment_tyranny, "social/economic/food_service").

domain_priors:requires_active_enforcement(condiment_tyranny).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(condiment_tyranny, large_food_service_operators).
narrative_ontology:constraint_beneficiary(condiment_tyranny, condiment_manufacturers).
narrative_ontology:constraint_beneficiary(condiment_tyranny, supply_chain_logistics).
narrative_ontology:constraint_victim(condiment_tyranny, consumer_choice_diversity).
narrative_ontology:constraint_victim(condiment_tyranny, niche_condiment_producers).
narrative_ontology:constraint_victim(condiment_tyranny, culinary_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NICHE CONDIMENT PRODUCER (SNARE) — Small-scale artisanal and regional condiment makers face insurmountable barriers to shelf space and table presence in standardized food service. Cannot exit without abandoning their entire market. The constraint extracts through distribution gatekeeping, volume requirements, and shelf-space monopolization by default brands. No meaningful alternatives or negotiating positions exist.
constraint_indexing:constraint_classification(condiment_tyranny, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL RESTAURANT OPERATOR (TANGLED ROPE) — Benefits from the standardized supply chain (reliable, low-cost sourcing; customer familiarity reduces decision friction). Also constrained by customer expectations for default options; deviation invites customer complaints and perception of cheapness. Partially trapped by switching costs and supplier relationships, but retains some agency to add specialty condiments alongside defaults. Mixed extraction and coordination.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE FOOD SERVICE CHAIN (ROPE) — Primary beneficiary. Standardization solves genuine coordination problems: economies of scale, supply chain optimization, customer recognition, cost reduction. The constraint functions as pure coordination from their perspective — they set the standard and profit from the coordination benefit. Can arbitrage by switching suppliers or adjusting offerings with minimal disruption.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONDIMENT MANUFACTURER (DEFAULT BRAND) (ROPE) — Secondary beneficiary. Standardization guarantees shelf presence and volume contracts. Coordination function: the constraint ensures their product is the default, which they optimize through distribution agreements and volume pricing. Pure coordination from their structural position — they are the primary extractors who set and benefit from the standard.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE DINING ECOSYSTEM (SCAFFOLD) — Farmers markets, food trucks, ethnic restaurants, premium casual chains, and online ordering platforms are creating parallel distribution channels with higher condiment diversity. These alternatives are gradually eroding the dominance of the default standard by providing low-friction access to specialty condiments. The constraint's extractiveness declines as alternatives mature. Sunset logic: as niche condiments move into mainstream supply chains (sriracha, harissa, chimichurri), the default standardization loses force.
constraint_indexing:constraint_classification(condiment_tyranny, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FAST-FOOD THEATER RITUAL (PITON) — The visible, ceremonial presentation of three condiment packets (ketchup, mustard, mayo) has become a performative marker of 'legitimate food service' even as its functional necessity has declined. Restaurants maintain the ritual through institutional inertia and customer expectation despite lower actual use. The constraint persists largely through theatrical maintenance rather than coordination necessity or extraction function — it is a degraded remnant of when condiment standardization solved real logistical problems.
constraint_indexing:constraint_classification(condiment_tyranny, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, standardization of condiment offerings appears as an inevitable consequence of supply chain economics at scale. The uniformity emerges from rational cost minimization and logistics optimization — it looks like a natural law of food service. However, the structural data contradicts this: the constraint is enforced through economic gatekeeping, not through physical necessity. This is a false summit — naturalizing a contingent economic arrangement.
constraint_indexing:constraint_classification(condiment_tyranny, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(condiment_tyranny_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(condiment_tyranny, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(condiment_tyranny, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(condiment_tyranny, TR),
    TR >= 0.70.

:- end_tests(condiment_tyranny_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through supply chain gatekeeping and cultural standardization, but not catastrophically — consumer exit is possible through alternative dining channels, and the extraction mechanism is economic rather than coercive. The value reflects that genuine coordination benefits exist alongside the extraction (economies of scale, reliability, reduced decision friction), making this a hybrid rather than pure extraction. Suppression (0.52): Moderate-high. Niche condiment producers face substantial barriers to distribution, but the barriers are not absolute — specialty channels (ethnic groceries, farmers markets, online platforms) exist outside the standardized system. Suppression operates through economic gatekeeping and cultural normalization rather than prohibition. Theater ratio (0.68): High and rising. The ritual of three condiment packets has become increasingly performative as online food delivery platforms reduce the logistical constraints that once made standardization functionally necessary. Restaurants continue offering default condiments more for cultural recognition than operational efficiency — it signals 'legitimate food service' rather than solving a real problem.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as pure coordination (large chain view), mixed extraction and coordination (individual restaurant view), pure extraction (niche producer view), temporary adjustment with sunset (alternative ecosystem view), and degraded theater (piton view). The large chain sees efficiency gains and solves real logistics problems. The niche producer sees gatekeeping and market exclusion. The analytical observer risks naturalizing a contingent economic arrangement as inevitable. The alternative dining ecosystem shows the constraint is eroding as viable exit paths mature. The piton perspective reveals that much of what appears as 'necessary standardization' is actually ritual maintenance — the constraint persists through institutional inertia more than functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Large chains and default manufacturers occupy beneficiary positions with arbitrage options (they can switch suppliers, adjust offerings, or create new standards). Niche producers are trapped with no viable exit outside the standardized system. Individual operators are constrained — they benefit from standardization but cannot easily deviate without customer dissatisfaction. The alternative ecosystem has mobile options (multiple distribution channels, multiple supply networks). The piton perspective derives from rising theater ratio: the constraint's performative content is increasing (ritual maintenance) while its functional necessity (solving coordination problems) is decreasing. This pattern indicates the constraint is aging rather than maturing — institutional inertia is replacing functional purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves mandatrophy by showing that standardization is simultaneously coordination AND extraction. The coordination function (supply chain efficiency) is genuine and substantial. The extraction function (gatekeeping niche producers, suppressing consumer choice diversity) is also genuine and substantial. The constraint cannot be decomposed into 'it is coordination' OR 'it is extraction' — it must be classified as hybrid. The rising theater ratio and emergence of viable alternatives suggest the constraint is aging: the coordination rationale for standardization is becoming less necessary (online platforms, modular supply chains), but the extraction mechanism persists through inertia. This is a tangled rope degrading toward piton, not a piton that was never rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_consumer_preference_shift,
    'At what point does accumulated consumer demand for diversity override the cost advantage of standardization?',
    'Time series analysis of specialty condiment adoption rates, consumer willingness-to-pay premiums, and platform data from restaurants that offer expanded options',
    'If threshold is low (~5-10% market adoption of alternatives): standardization will collapse rapidly as supply chains adapt. If threshold is high (>30% adoption): default offering persists through supplier lock-in despite latent demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_consumer_preference_shift, empirical, 'Consumer preference shift threshold for condiment diversity').

omega_variable(
    supply_chain_economies_necessity,
    'Are the economies of scale in standardized condiment sourcing intrinsically necessary, or are they artifacts of consolidation that could dissolve if supply chains became more modular?',
    'Cost comparison between centralized vs distributed condiment sourcing models; analysis of specialty supply platforms (e.g., farmers market aggregators, food service cooperatives) achieving comparable economies',
    'If necessary: standardization is partially a mountain (structural limit to distributed sourcing). If artifact: standardization is pure extraction, and alternative chains can achieve cost parity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_economies_necessity, empirical, 'Whether supply chain economies mandate condiment standardization').

omega_variable(
    cultural_authenticity_extraction_value,
    'Does offering cuisine-authentic condiments provide sufficient value differentiation to justify premium pricing that compensates for higher supply chain costs?',
    'Comparative pricing and customer retention analysis for restaurants with expanded condiment offerings vs standardized chains; survey data on condiment selection as purchasing factor',
    'If yes: premium positioning is viable exit from default constraint. If no: niche producers cannot build sustainable business models outside standardized channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_authenticity_extraction_value, empirical, 'Value of cuisine-authentic condiments as premium differentiator').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(condiment_tyranny, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cond_tr_t0, condiment_tyranny, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cond_tr_t15, condiment_tyranny, theater_ratio, 15, 0.58).
narrative_ontology:measurement(cond_tr_t30, condiment_tyranny, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cond_be_t0, condiment_tyranny, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cond_be_t15, condiment_tyranny, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(cond_be_t30, condiment_tyranny, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(condiment_tyranny, resource_allocation).
narrative_ontology:affects_constraint(condiment_tyranny, flavor_profile_cultural_suppression).
narrative_ontology:affects_constraint(condiment_tyranny, supply_chain_consolidation_barriers).

% DUAL FORMULATION NOTE:
% The condiment tyranny is distinct from but related to broader supply chain consolidation and food industrialization. The upstream constraint (supply chain consolidation) creates the conditions enabling the downstream constraint (condiment standardization). The alternative dining ecosystem represents a structural escape route from both constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
