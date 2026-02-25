% ============================================================================
% CONSTRAINT STORY: china_rare_earth_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_rare_earth_dominance, []).

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
 *   constraint_id: china_rare_earth_dominance
 *   human_readable: China's Strategic Dominance of the Rare Earth Element Market
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint describes China's strategic control over the global
 *   supply of rare earth elements (REEs), which are critical for renewable
 *   energy and high-tech manufacturing. While having abundant reserves is one
 *   factor, China's dominance stems from its control over the entire complex
 *   and polluting processing supply chain. This creates a near-monopoly that
 *   allows for price manipulation and exerts geopolitical leverage,
 *   effectively suppressing the emergence of competitors like Brazil and
 *   India and creating strategic vulnerabilities for consumer nations.
 *
 * KEY AGENTS:
 *   - Chinese State-Owned Enterprises: Primary beneficiary (institutional/arbitrage) — Control production, processing, and pricing.
 *   - Aspiring Producer Nations (Brazil, India): Primary victim (institutional/constrained) — Possess reserves but are locked out of the market by high barriers to entry.
 *   - Western Consumer Nations (US, EU): Secondary victim (powerful/mobile) — Reliant on Chinese supply, facing strategic risk.
 *   - Global Tech Manufacturers: Victim (powerful/mobile) — Require REEs for products and are exposed to supply/price shocks.
 *   - Local Mining Communities: Potential victim (powerless/trapped) — Face severe environmental damage from mining operations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_rare_earth_dominance, 0.62).
domain_priors:suppression_score(china_rare_earth_dominance, 0.75).
domain_priors:theater_ratio(china_rare_earth_dominance, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_rare_earth_dominance, extractiveness, 0.62).
narrative_ontology:constraint_metric(china_rare_earth_dominance, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(china_rare_earth_dominance, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_rare_earth_dominance, tangled_rope).
narrative_ontology:human_readable(china_rare_earth_dominance, "China's Strategic Dominance of the Rare Earth Element Market").
narrative_ontology:topic_domain(china_rare_earth_dominance, "geopolitical/economic").

domain_priors:requires_active_enforcement(china_rare_earth_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_rare_earth_dominance, chinese_state_owned_enterprises).
narrative_ontology:constraint_beneficiary(china_rare_earth_dominance, chinese_strategic_planners).
narrative_ontology:constraint_victim(china_rare_earth_dominance, aspiring_producer_nations).
narrative_ontology:constraint_victim(china_rare_earth_dominance, western_consumer_nations).
narrative_ontology:constraint_victim(china_rare_earth_dominance, global_tech_manufacturers).
narrative_ontology:constraint_victim(china_rare_earth_dominance, local_mining_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PRODUCER (SNARE) — From the perspective of a nation with REE reserves but lacking processing capacity, the market is a trap. China's ability to manipulate prices and control the complex processing supply chain makes any new large-scale investment economically non-viable, suppressing competition. Despite being an institutional actor, their exit option is 'constrained' by these high barriers. d from victim+constrained exit -> high d -> high χ.
constraint_indexing:constraint_classification(china_rare_earth_dominance, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: MARKET DOMINATOR (ROPE) — From China's perspective, its control of the REE market is a massive coordination success, providing a stable, integrated supply chain for global manufacturing. The strategic pricing and export controls are seen as prudent management of a critical resource, not extraction. As the primary beneficiary with arbitrage options, they experience negative effective extraction (χ < 0).
constraint_indexing:constraint_classification(china_rare_earth_dominance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DEPENDENT CONSUMER (TANGLED ROPE) — For Western nations and companies reliant on REEs, the situation is a hybrid. They benefit from the current coordinated supply but are victims of the strategic vulnerability and price volatility it creates. Their 'mobile' exit option represents their ongoing efforts to diversify the supply chain by funding projects in Brazil and elsewhere, acknowledging both the coordination and extraction aspects.
constraint_indexing:constraint_classification(china_rare_earth_dominance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: LOCAL COMMUNITY (SNARE) — For local populations where new REE mines might be developed (e.g., in Brazil), the constraint is a pure extractive threat. They face the severe environmental costs of mining (radioactive waste, water pollution) for geopolitical benefits they will likely never see. They are trapped by economic circumstance and bear uncompensated costs. d≈0.95, f(d)≈1.42 -> high χ.
constraint_indexing:constraint_classification(china_rare_earth_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees the full structure: a system that performs a genuine coordination function (integrating a complex supply chain) but is coupled with extremely high, actively enforced extraction (monopoly pricing, geopolitical leverage, suppression of alternatives). The base metrics (ε=0.62, suppression=0.75) confirm this is not a pure Rope or Snare, but a hybrid.
constraint_indexing:constraint_classification(china_rare_earth_dominance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_rare_earth_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_rare_earth_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_rare_earth_dominance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_rare_earth_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_rare_earth_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.62) is high, reflecting the significant economic rent and geopolitical power extracted through monopoly control. Suppression (0.75) is very high due to active market strategies, such as price dumping, that make it nearly impossible for new entrants to compete profitably. The Theater Ratio (0.20) is low, as this is a highly functional system of industrial and strategic control, not a performative one. The system requires active enforcement through state policy, export quotas, and strategic investments, meeting a key gate for a Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. China views its market position as a Rope — an efficient, well-managed coordination mechanism. Aspiring producers like Brazil experience it as a Snare — an inescapable economic trap that stifles their development. Dependent Western nations see a Tangled Rope — they benefit from the supply but are acutely aware of the extractive risks. This gap highlights the difference between managing a supply chain and weaponizing it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Chinese state actors are the declared beneficiaries, and their arbitrage power gives them a low 'd' value, making the constraint appear beneficial (Rope). All other actors are victims. Aspiring producer nations, despite being institutional, have constrained exit options and thus a high 'd', perceiving a Snare. Powerless local communities have the highest 'd' of all. The engine correctly derives these divergent classifications from the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves a potential mandatrophy by refusing to classify a strategic monopoly as simple market coordination (Rope). By identifying both the genuine coordination function (an integrated supply chain) and the severe, actively enforced extraction, the framework correctly identifies the structure as a Tangled Rope. It demonstrates that the 'Rope' classification is a beneficiary's perspective, not an objective description of the full system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_of_alternatives,
    'Can new producers like Brazil and India realistically achieve the scale and technological capacity to break China''s monopoly, or will they always be vulnerable to strategic price dumping?',
    'Economic modeling of market entry costs vs. China''s production costs; empirical results from the first few years of operation of new processing plants funded by Western nations.',
    'If alternatives are viable, the constraint''s suppression and extractiveness will decrease, potentially shifting it towards a Rope. If not, it remains a durable Snare for new entrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_of_alternatives, empirical, 'Whether non-Chinese REE supply chains can become economically viable at scale').

omega_variable(
    technological_substitution,
    'Will technological innovation create viable, scalable alternatives to rare-earth magnets and other REE-dependent components, thereby collapsing demand?',
    'Tracking materials science research and development in permanent magnets and catalysts. Monitoring adoption rates of REE-free technologies in EV motors and wind turbines.',
    'A breakthrough would fundamentally reduce the constraint''s power, making it a Piton as nations compete over a less critical resource. Failure to find substitutes solidifies its power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_substitution, empirical, 'The potential for technology to engineer-out the dependency on rare earth elements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_rare_earth_dominance, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t1990, china_rare_earth_dominance, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(chin_tr_t2007, china_rare_earth_dominance, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(chin_tr_t2024, china_rare_earth_dominance, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(chin_be_t1990, china_rare_earth_dominance, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(chin_be_t2007, china_rare_earth_dominance, base_extractiveness, 2007, 0.5).
narrative_ontology:measurement(chin_be_t2024, china_rare_earth_dominance, base_extractiveness, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_rare_earth_dominance, resource_allocation).
narrative_ontology:affects_constraint(china_rare_earth_dominance, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_rare_earth_dominance, global_renewable_energy_transition).
narrative_ontology:affects_constraint(china_rare_earth_dominance, electric_vehicle_manufacturing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
