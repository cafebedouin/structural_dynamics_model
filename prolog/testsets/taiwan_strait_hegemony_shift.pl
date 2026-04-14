% ============================================================================
% CONSTRAINT STORY: taiwan_strait_hegemony_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_strait_hegemony_shift, []).

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
 *   constraint_id: taiwan_strait_hegemony_shift
 *   human_readable: The Taiwan Strait Energy & Logistics Chokepoint
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Taiwan Strait Energy & Logistics Chokepoint represents a structural
 *   constraint on global trade and energy security created by geographic
 *   concentration and geopolitical hegemonic competition. Approximately 30%
 *   of global maritime trade ($15 trillion in annual flows) transits the
 *   strait, including 90% of LNG destined for East Asia and critical
 *   semiconductor supply chains. Should a single hegemonic power achieve
 *   controlling dominance over Taiwan (either through military unification or
 *   strategic alignment), that power would inherit an unparalleled leverage
 *   point over global energy prices, semiconductor availability, and economic
 *   coercion capacity. The constraint exhibits characteristics of a Snare —
 *   high base extractiveness (0.68), severe suppression of alternatives
 *   (0.72), and minimal theater (0.38, indicating functional rather than
 *   performative extraction mechanisms). However, the constraint also shows
 *   perspectival variation: regional actors experience it as Tangled Rope
 *   (coordination benefits + extraction), while alternative energy and
 *   logistics coalitions see it as Scaffold (temporary, degrading over 20-30
 *   years as alternatives mature). International maritime law appears as
 *   Piton (degraded normative force, performative commitment to freedom of
 *   navigation). The analytical observer sees structural Snare at the
 *   civilizational level: current global supply chains have no escape
 *   velocity without radical reconfiguration. Theater ratio is low and
 *   declining, indicating the constraint operates through direct functional
 *   mechanisms (chokepoint rent, strategic leverage, supply disruption
 *   threat) rather than through performative institutions.
 *
 * KEY AGENTS:
 *   - Hegemonic Controlling Power (institutional/arbitrage) — primary beneficiary; gains strategic leverage, energy price control, revenue from transit/enforcement mechanisms
 *   - Global Shipping & Energy Trade (powerless/trapped) — primary victim; cannot exit strait without 16-25% transit cost increase; bears extraction through tolls, delays, vulnerability
 *   - Energy-Dependent Nations (moderate/constrained) — secondary victims; Japan, South Korea, India, Europe dependent on Persian Gulf oil transiting strait; face tariff escalation and strategic vulnerability
 *   - Regional Economic Actors (powerful/mobile) — tertiary beneficiaries/victims; Taiwan, Singapore, Hong Kong benefit from transshipment fees but also constrained by hegemonic control
 *   - Alternative Energy Coalition (organized/mobile) — sunset agents; developing Arctic routes, LNG alternatives, renewable localization to degrade extractiveness over 20-30 years
 *   - International Maritime Law Institutions (institutional/arbitrage) — performative; UNCLOS and maritime norms theoretically constrain extraction but lack enforcement; appear as Piton
 *   - Analytical Observer (analytical/analytical) — sees structural Snare at systemic level; current global supply chains lack meaningful exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, 0.68).
domain_priors:suppression_score(taiwan_strait_hegemony_shift, 0.72).
domain_priors:theater_ratio(taiwan_strait_hegemony_shift, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, extractiveness, 0.68).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_strait_hegemony_shift, snare).
narrative_ontology:human_readable(taiwan_strait_hegemony_shift, "The Taiwan Strait Energy & Logistics Chokepoint").
narrative_ontology:topic_domain(taiwan_strait_hegemony_shift, "political/economic").

domain_priors:requires_active_enforcement(taiwan_strait_hegemony_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_strait_hegemony_shift, controlling_hegemonic_power).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, global_shipping_lanes_users).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, regional_economies_outside_hegemony).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, semiconductor_supply_chains).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, energy_trade_dependent_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SHIPPING & ENERGY TRADE (SNARE) — Cannot exit the strait without massive rerouting costs (16-25% longer transit times via Sunda Strait). Bears full extraction through toll mechanisms, inspection delays, and chokepoint rent. Maximum experienced extraction — abstract epistemic commons (efficient global supply chains) has no advocate. Trapped exit: rerouting is theoretically available but economically catastrophic.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY-DEPENDENT NATIONS (SNARE) — Japan, South Korea, India, Europe dependent on Persian Gulf oil and LNG transiting strait. Face extraction through tariff escalation, shipping insurance premiums, strategic vulnerability to supply disruption. Constrained exit: can shift to alternatives (renewable, Russian LNG pre-sanctions model, local production) but transition takes 10-20 years and requires capital investment. Meanwhile, extraction is severe and non-negotiable.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ACTORS (TANGLED ROPE) — Taiwan, Singapore, Hong Kong benefit from strait geography through transshipment fees, financial services, and strategic positioning. But also dependent on strait remaining open for their own exports and imports. Hegemonic control extracts through rent and privilege access. Regional actors experience coordination function (strait remains stable, predictable) AND asymmetric extraction (must accept higher costs/restrictions than hegemonic power). Mobile exit available (shift financial/port activity elsewhere) but involves competitive disadvantage.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: HEGEMONIC CONTROLLING POWER (ROPE) — Primary beneficiary. Gains revenue from transit fees, strategic leverage over energy prices, ability to sanction without self-harm. For the hegemonic power, the constraint appears as pure coordination: strait management, collection mechanisms, enforcement against rival powers. Net beneficiary with maximum arbitrage (can exclude competitors, set terms unilaterally). Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE PATHWAYS COALITION (SCAFFOLD) — Organized efforts to develop alternative supply routes (Arctic shipping, Suez alternatives, renewable energy localization, LNG diversification) represent a sunset mechanism. Coalition members (Norway, renewable energy investors, pipeline infrastructure projects) see the strait's extractive rent as temporary. High suppression initially (entrenched interests, geopolitical risk, capital requirements) but declining over 20-30 year horizon as technologies mature. Theater low for this perspective — the functional alternative pathways are being built, not merely performed.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME LAW (PITON) — UNCLOS and international maritime norms theoretically constrain hegemonic extraction and guarantee freedom of navigation. But enforcement has degraded (major powers ignore rulings, norms are theater, dispute mechanisms are performative). The rule-of-law framework persists through institutional inertia despite its low functional force. Theater ratio high (declarations of commitment to free trade, UNCLOS lip service, symbolic naval patrols) masking actual extraction.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational and universal scope, the strait is a structural chokepoint of the current global system: ~30% of maritime trade, ~$15 trillion in annual flows, 90% of East Asian energy imports. No natural alternative exists at equivalent scale without radical reconfiguration of supply chains. Hegemonic control over such a chokepoint is inherently extractive at the systemic level. The constraint is not 'natural' but is quasi-structural given current energy and trade patterns. No meaningful exit for the global system except through multi-decadal transition to alternatives.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_strait_hegemony_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_strait_hegemony_shift, TR),
    TR >= 0.70.

:- end_tests(taiwan_strait_hegemony_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Hegemonic control over the Taiwan Strait creates unprecedented leverage over global energy and trade flows. The rent extraction available through tariff mechanisms, strategic supply disruption threats, and selective access control is substantial — potentially 3-8% of trade value transiting the strait. The extractiveness trajectory increases from 0.35 to 0.68 over the 20-year interval as hegemonic control solidifies, alternative routes remain underdeveloped, and dependency deepens. Suppression (0.72): High. Alternatives to the strait are economically and technically constrained: rerouting through Sunda Strait adds 8-10 days transit and 16-25% fuel costs; Arctic routes require infrastructure investment and remain climate-dependent; renewable energy localization requires 15-25 year transitions; pipeline alternatives (Nabucco, etc.) face geopolitical fragmentation. Victims face trapped or constrained exit, increasing suppression. Theater ratio (0.38): Low and declining. The extraction mechanism operates through direct functional leverage (chokepoint rent, supply threat credibility) rather than through performative institutions. International maritime law theater (UNCLOS declarations, freedom of navigation norms) provides cover but lacks enforcement; functional mechanisms dominate. Low theater indicates high structural authenticity — the snare is real, not camouflaged.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests radically differently across structural positions. The hegemonic controlling power experiences Rope: they see the strait as a solved coordination problem, a functional system they manage for revenue and strategic leverage. Their exit options are arbitrage — they set terms and exclude competitors. Regional beneficiaries (Singapore, Hong Kong) experience Tangled Rope: they benefit from strait stability and transshipment fees, but face extraction through hegemonic privilege and restricted access. Their mobile exit (shift to alternative ports) is available but costly. Trapped victims (global shipping, energy-dependent nations) experience maximum Snare: they cannot exit without catastrophic cost increases. For the analytical observer at civilizational scope, the constraint becomes a structural feature of the current global system — not a Snare created by hegemony, but an immutable chokepoint of the architecture itself. The Piton classification of international maritime law reveals the degradation of normative constraints: UNCLOS theater masks functional powerlessness. The Scaffold perspective identifies the sunset mechanism: alternative routes and renewable localization create a genuine long-term escape path, but it operates on a 20-30 year horizon, not an immediate one.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to extraction flow. The hegemonic controlling power has d ≈ 0.05 (full beneficiary): gains flow in reverse, experiences negative effective extraction, arbitrage exit options. Derived f(d) ≈ -0.12, canonical institutional psychology. Regional actors have d ≈ 0.50-0.55 (mixed): some benefit from strait stability, some bear extraction through privilege restrictions; mobile exit gives them agency but incomplete escape. Derived f(d) ≈ 0.65-0.75, consistent with 'powerful' psychology. Global shipping and energy-dependent nations have d ≈ 0.85-0.95 (full targets): extraction flows entirely toward them, trapped exit eliminates agency. Derived f(d) ≈ 1.15-1.42, consistent with 'powerless' psychology. The analytical observer has d ≈ 0.73 (analytical distance): sees structure without bearing extraction, derives f(d) ≈ 1.15. Effective extraction χ = ε × f(d) × σ(S) produces maximum χ for trapped victims (high ε, high f(d), global scope σ=1.2), moderate χ for regional actors, minimum/negative χ for hegemonic beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids false classification through clear beneficiary/victim decomposition and perspectival analysis. The risk is misclassifying the constraint as 'coordination' (rope) because international maritime law and UNCLOS provide normative scaffolding. However, the base properties reveal this is false coordination: suppression is high (0.72), theater is low (0.38), beneficiaries are concentrated (single hegemonic power), and victims are dispersed (global economy). True coordination would have suppression < 0.40, shared beneficiary interests, and escape options. The constraint classifies as Snare from the trapped agent perspective because the fundamental asymmetry is extraction, not coordination. The Rope perspective from the hegemonic power is their subjective experience, not the constraint's objective structure. The Piton classification of maritime law norms reflects the degradation of normative constraints: the international rules-based order persists through theater, not through functional force, revealing that legal coordination is performative rather than real. The Scaffold perspective validates the snare classification by identifying the genuine exit path (alternatives) as slow (20-30 years) and uncertain, confirming that current victims remain trapped in the immediate term.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hegemonic_power_identity,
    'Which power assumes controlling hegemony? Chinese unification, US renewed containment, or multipolar fragmentation?',
    'Taiwan political status resolution; naval force projection capabilities in 2030-2050; international consensus on strait governance',
    'If China: extraction mechanism targets US allies and Western economies; snare classification hardened. If US-secured: classification shifts toward rope/scaffold for US allies; snare deepens for China. If multipolar: multiple overlapping snares (contested control, insurance complexity); classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemonic_power_identity, preference, 'Which power achieves controlling hegemony over the strait').

omega_variable(
    alternative_route_feasibility,
    'Can Arctic routes (NSR), Suez alternatives (Nabucco pipelines), or renewable localization achieve cost parity with Taiwan Strait before 2045?',
    'Cost trajectory analysis of Arctic shipping infrastructure; LNG production capacity shifts; renewable energy cost curves and grid stability solutions; geopolitical fragmentation of alternative routes',
    'If yes by 2040: scaffold sunset confirmed, constraint degrades to piton by 2050. If no: extractiveness remains high indefinitely, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_route_feasibility, empirical, 'Technical and economic feasibility of alternative supply routes').

omega_variable(
    enforceability_of_extraction,
    'Can hegemonic power actually enforce chokepoint extraction without provoking military response or coalition formation against it?',
    'Historical precedent analysis (Suez 1956, Panama 1989, Malacca ASEAN agreements); simulation of coalition response thresholds; cost-benefit analysis for would-be blockers',
    'If enforcement fragile: effective extraction χ drops significantly even with high base ε and suppression, reclassification toward tangled_rope. If enforceable: snare classification confirmed at highest χ levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_of_extraction, empirical, 'Whether extraction can be enforced without triggering military counter-response').

omega_variable(
    rent_extraction_magnitude,
    'What is the actual magnitude of extractive rent available from hegemonic control? Is it 1-2% of trade value (minimal) or 5-10% (severe)?',
    'Model of chokepoint pricing power: comparison with Panama Canal tolls, Suez fees, Singapore port premiums; gaming theory of demand elasticity under supply disruption threats',
    'If < 3%: extractiveness should be revised downward to 0.45-0.50 (moderate snare/tangled rope boundary). If > 8%: confirms extractiveness 0.68 and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_extraction_magnitude, empirical, 'Quantitative magnitude of available extractive rent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_strait_hegemony_shift, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tw_strait_tr_t0, taiwan_strait_hegemony_shift, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tw_strait_tr_t10, taiwan_strait_hegemony_shift, theater_ratio, 10, 0.42).
narrative_ontology:measurement(tw_strait_tr_t20, taiwan_strait_hegemony_shift, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(tw_strait_be_t0, taiwan_strait_hegemony_shift, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tw_strait_be_t10, taiwan_strait_hegemony_shift, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tw_strait_be_t20, taiwan_strait_hegemony_shift, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_strait_hegemony_shift, global_infrastructure).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, lng_price_volatility_transmission).
narrative_ontology:affects_constraint(taiwan_strait_hegemony_shift, geopolitical_alliance_fragmentation).

% DUAL FORMULATION NOTE:
% The Taiwan Strait Chokepoint decomposes into two related but distinct constraints: (1) Geographic chokepoint as structural feature (this story, ε=0.68, snare/scaffold family), and (2) Hegemonic power projection as institutional arrangement (distinct story, ε higher if hegemony is enforced, lower if contested). This story models the chokepoint itself assuming hegemonic control has been achieved. A companion story modeling contested control or multipolar fragmentation would have different ε (likely 0.75+, higher suppression, more snare from all perspectives). The network affects show downstream constraints that depend on strait control: semiconductor supply chains experience direct extraction, LNG pricing becomes hegemony-dependent, alliance stability depends on chokepoint rent distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_strait_hegemony_shift, institutional, 0.05).
constraint_indexing:directionality_override(taiwan_strait_hegemony_shift, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
