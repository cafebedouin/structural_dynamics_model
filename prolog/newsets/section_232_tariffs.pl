% ============================================================================
% CONSTRAINT STORY: section_232_tariffs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_section_232_tariffs, []).

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
 *   constraint_id: section_232_tariffs
 *   human_readable: Section 232 National Security Tariffs
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint models the tariffs on steel and aluminum imports imposed
 *   by the U.S. under Section 232 of the Trade Expansion Act of 1962. The
 *   official justification for these tariffs was national security, arguing
 *   that a robust domestic metals industry is essential for defense
 *   capabilities. However, the policy functions primarily as a protectionist
 *   measure, shielding domestic producers from foreign competition. This
 *   creates a structural conflict between the protected industries, which
 *   benefit from higher prices, and the downstream manufacturers and end
 *   consumers, who bear the increased costs.
 *
 * KEY AGENTS:
 *   - U.S. Steel/Aluminum Producers: Primary beneficiaries (institutional/arbitrage) who gain market share and pricing power.
 *   - Downstream Manufacturers: Primary victims (organized/constrained) such as automakers and appliance companies, who face higher input costs.
 *   - U.S. Consumers: Secondary victims (powerless/trapped) who pay higher prices for finished goods.
 *   - The U.S. Executive Branch: The institutional enforcer of the policy, which claims a national security coordination function.
 *   - Foreign Exporters: Victims who are priced out of the U.S. market.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(section_232_tariffs, 0.65).
domain_priors:suppression_score(section_232_tariffs, 0.7).
domain_priors:theater_ratio(section_232_tariffs, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(section_232_tariffs, extractiveness, 0.65).
narrative_ontology:constraint_metric(section_232_tariffs, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(section_232_tariffs, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(section_232_tariffs, tangled_rope).
narrative_ontology:human_readable(section_232_tariffs, "Section 232 National Security Tariffs").
narrative_ontology:topic_domain(section_232_tariffs, "economic/political").

domain_priors:requires_active_enforcement(section_232_tariffs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(section_232_tariffs, us_steel_producers).
narrative_ontology:constraint_beneficiary(section_232_tariffs, us_aluminum_producers).
narrative_ontology:constraint_beneficiary(section_232_tariffs, associated_labor_unions).
narrative_ontology:constraint_victim(section_232_tariffs, downstream_manufacturers).
narrative_ontology:constraint_victim(section_232_tariffs, us_consumers).
narrative_ontology:constraint_victim(section_232_tariffs, foreign_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM MANUFACTURER (SNARE) — An automobile or appliance maker now forced to buy more expensive domestic steel. Their input costs rise, making them less competitive globally. Exit options are constrained; moving factories is a massive undertaking. From this view, the tariff is a pure extractive trap. d derived from victim + constrained, χ is high.
constraint_indexing:constraint_classification(section_232_tariffs, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTECTED PRODUCER (ROPE) — A domestic steel mill that can now charge higher prices due to suppressed foreign competition. They see the tariff as a fair and necessary coordination mechanism to ensure national security and a level playing field. Their institutional power and arbitrage exit (lobbying) gives them a low d, resulting in negative effective extraction.
constraint_indexing:constraint_classification(section_232_tariffs, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE ENFORCER (SCAFFOLD) — The government publicly frames the tariffs as a temporary support structure needed to rebuild a vital domestic industry for national defense. The policy is presented with an implicit sunset clause: it's only necessary until the industry is secure again. This perspective captures the official justification.
constraint_indexing:constraint_classification(section_232_tariffs, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — An economist or trade lawyer sees the dual function. There is a claimed (if weak) coordination purpose (national security) alongside a very strong, asymmetric extraction of wealth from consumers and downstream industries to protected producers. The high ε, high suppression, and active enforcement make it a textbook Tangled Rope.
constraint_indexing:constraint_classification(section_232_tariffs, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: END CONSUMER (SNARE) - The individual buying a car or washing machine faces a higher price due to the increased cost of steel. They are powerless to avoid this cost and are trapped by the national scope of the policy. The national security justification is abstract and provides no direct benefit, making the price increase feel purely extractive.
constraint_indexing:constraint_classification(section_232_tariffs, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(section_232_tariffs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(section_232_tariffs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(section_232_tariffs, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(section_232_tariffs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(section_232_tariffs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high because the tariffs engineer a direct transfer of wealth from a broad base of consumers and industries to a concentrated group of producers. Suppression (0.70) is high, as the explicit purpose is to suppress the alternative of importing cheaper materials. The Theater Ratio (0.60) reflects the widespread view among economists that the 'national security' justification is a pretext for economic protectionism, making a significant portion of the policy's rationale performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The protected domestic producer experiences a beneficial Rope, a simple rule that helps them. The downstream manufacturer, whose business is harmed by inflated costs, experiences an inescapable Snare. The government frames its own action as a temporary Scaffold for a vital industry. The analytical observer, weighing the claimed coordination function against the obvious extraction, classifies it as a Tangled Rope. This highlights how a single policy can be perceived as helpful, harmful, or a complex hybrid depending on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation is driven by the clear beneficiary/victim structure. Beneficiaries ('us_steel_producers') are institutional actors with arbitrage exit (they can lobby to maintain or alter the rules), which yields a low 'd' value and a Rope classification. Victims ('downstream_manufacturers') are organized but have constrained exit (they cannot easily move production), which yields a high 'd' and a Snare classification. The system correctly models that the same policy can be simultaneously a subsidy and a tax depending on the agent's relationship to it.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of potential mandatrophy, where a policy claimed to serve the collective good (national security) is structurally an extractive mechanism for a select few. The framework resolves this by distinguishing between the claimed function and the analytical reality. While the executive claims a Scaffold, the high, asymmetric extraction (ε=0.65) and lack of a formal sunset clause lead the analytical observer to correctly identify it as a Tangled Rope, while acknowledging the Snare experienced by its targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_pretext,
    'Is the national security justification a genuine strategic necessity or a pretext for economic protectionism?',
    'Declassified defense assessments of domestic steel/aluminum production capacity versus actual, quantified military procurement needs during a major conflict.',
    'If genuine, the constraint has a stronger coordination component, validating the Tangled Rope classification. If a pretext, it is structurally a Snare disguised as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_pretext, empirical, 'Whether the national security claim is genuine or a pretext for protectionism').

omega_variable(
    net_economic_effect,
    'Do the economic benefits to the protected industries (jobs, profits) outweigh the costs to downstream industries and consumers (higher prices, job losses)?',
    'Comprehensive macroeconomic modeling of job gains/losses, price effects, and GDP impact across all affected sectors over the policy''s lifecycle.',
    'A net-negative economic effect reinforces the Snare classification from a societal perspective. A net-positive effect (which most economic studies dispute) would bolster the Tangled Rope/Scaffold case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_economic_effect, empirical, 'The net economic impact of the tariffs on the overall US economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(section_232_tariffs, 2018, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sect_tr_t2018, section_232_tariffs, theater_ratio, 2018, 0.5).
narrative_ontology:measurement(sect_tr_t2021, section_232_tariffs, theater_ratio, 2021, 0.55).
narrative_ontology:measurement(sect_tr_t2024, section_232_tariffs, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(sect_be_t2018, section_232_tariffs, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(sect_be_t2021, section_232_tariffs, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(sect_be_t2024, section_232_tariffs, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(section_232_tariffs, resource_allocation).
narrative_ontology:affects_constraint(section_232_tariffs, wto_dispute_settlement_system).
narrative_ontology:affects_constraint(section_232_tariffs, global_supply_chain_fragility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
