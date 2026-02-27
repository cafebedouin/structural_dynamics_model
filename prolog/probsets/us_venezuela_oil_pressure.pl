% ============================================================================
% CONSTRAINT STORY: us_venezuela_oil_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_venezuela_oil_pressure, []).

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
 *   constraint_id: us_venezuela_oil_pressure
 *   human_readable: US Geopolitical & Economic Pressure on Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the decades-long United States policy of exerting
 *   diplomatic, economic, and covert pressure on Venezuela to ensure its vast
 *   oil reserves remain accessible and favorable to US corporate and
 *   geopolitical interests. This has taken the form of sanctions, support for
 *   opposition movements, and diplomatic isolation. The pressure has
 *   significantly impacted the Venezuelan economy and society.
 *
 * KEY AGENTS:
 *   - US Oil Companies: Primary beneficiary (institutional/arbitrage)
 *   - US Government: Secondary beneficiary (institutional/mobile)
 *   - Venezuelan People: Primary victim (powerless/trapped)
 *   - Venezuelan Government: Secondary victim (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_oil_pressure, 0.75).
domain_priors:suppression_score(us_venezuela_oil_pressure, 0.85).
domain_priors:theater_ratio(us_venezuela_oil_pressure, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_venezuela_oil_pressure, snare).
narrative_ontology:human_readable(us_venezuela_oil_pressure, "US Geopolitical & Economic Pressure on Venezuela's Oil Sector").
narrative_ontology:topic_domain(us_venezuela_oil_pressure, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_venezuela_oil_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_oil_companies).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_government).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_people).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_government).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Venezuelan people are trapped within a collapsing economy exacerbated by sanctions, with limited exit options and bearing the brunt of the pressure.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Venezuelan government is constrained by US pressure but also benefits from oil revenues (albeit diminishing) and retains some agency through alliances and resourcefulness. Extraction is high, but not absolute.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% US oil companies benefit from access to Venezuelan oil resources, experiencing the constraint as a facilitator for resource extraction and market dominance. Arbitrage is available through global markets and shifting alliances.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Other nations dependent on oil may find that the actions of the United States constrains their exit options with regard to oil supplies and their price.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The US Government benefits from exerting influence over Venezuelan oil resources, maintaining geopolitical leverage in the region, but also faces constraints from international relations and domestic politics. The effectiveness is mixed.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational perspective, the analytical observer sees a Tangled Rope where US pressure achieves its geopolitical and economic goals, extracting resources and influence, but with significant costs to the Venezuelan people and potential long-term damage to international relations.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_oil_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_venezuela_oil_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_venezuela_oil_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The US extracts significant economic and geopolitical benefits from exerting pressure on Venezuela. The Venezuelan economy has suffered greatly, and the US gains favorable access to oil resources and maintains its influence in the region. Suppression (0.85): High. The US actively suppresses alternative political and economic models in Venezuela, limiting the country's options and maintaining its dependence. The US suppresses other countries doing business with Venezuela as well, making this a high suppression item. Theater Ratio (0.30): Relatively low, although some is performative. Public diplomacy efforts and justifications often mask underlying strategic objectives.
 *
 * PERSPECTIVAL GAP:
 *   The Venezuelan people experience the constraint as a pure Snare, with limited exit options and bearing the brunt of the pressure. The Venezuelan government sees a Tangled Rope, constrained by US pressure but also retaining some agency. US oil companies experience the constraint as a facilitator for resource extraction and market dominance, a Rope. The US government views the constraint as achieving its geopolitical and economic goals, a Tangled Rope. The analytical observer recognizes the long-term costs and consequences, a Tangled Rope, while the sanctions placed on trade constrains other nations oil supplies.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the power and exit options of each agent. The Venezuelan people are powerless and trapped, experiencing high extraction. US oil companies have arbitrage options and benefit, experiencing low or negative extraction. The US government has moderate power and mobility, experiencing mixed results. International power relations are the primary determinant of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The high extractiveness is justified by the severe impact on the Venezuelan people and economy. While the US government and oil companies benefit, the costs to Venezuela are disproportionately high, making the snare classification appropriate. The alternative perspectives highlight the different experiences of the actors involved, but the overall structure remains extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_oil_sources,
    'To what extent can alternative oil sources and energy technologies reduce global dependence on Venezuelan oil, diminishing the US geopolitical rationale?',
    'Analysis of global energy market trends, technological advancements in renewable energy, and shifts in geopolitical alliances.',
    'If dependence decreases significantly, the constraint shifts towards a scaffold, as the US incentive for pressure diminishes. If dependence remains high, the constraint solidifies as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_oil_sources, empirical, 'The availability of alternative oil sources affects the US geopolitical rationale.').

omega_variable(
    venezuelan_government_stability,
    'What is the probability of a regime change in Venezuela that aligns with US interests?',
    'Political risk analysis, assessment of internal and external pressures on the Venezuelan government, and evaluation of potential alternative leadership.',
    'If a pro-US regime emerges, the constraint could transform into a rope, representing a coordinated relationship. If the current regime persists or a hostile regime rises, the constraint will solidify as a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(venezuelan_government_stability, empirical, 'The stability of the Venezuelan government impacts the US strategy.').

omega_variable(
    international_condemnation_threshold,
    'At what point does international condemnation of US pressure on Venezuela outweigh the perceived benefits of resource control?',
    'Assessment of UN resolutions, diplomatic statements from key nations, and the effectiveness of Venezuelan lobbying efforts.',
    'If international pressure becomes overwhelming, the constraint may shift towards a piton, as the US faces increasing isolation and the policy becomes unsustainable. If the US is able to withstand criticism, the constraint will remain a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_condemnation_threshold, preference, 'The international condemnation of the US affects policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_venezuela_oil_pressure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_v_tr_t0, us_venezuela_oil_pressure, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_v_tr_t10, us_venezuela_oil_pressure, theater_ratio, 10, 0.2).
narrative_ontology:measurement(us_v_tr_t20, us_venezuela_oil_pressure, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(us_v_be_t0, us_venezuela_oil_pressure, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_v_be_t10, us_venezuela_oil_pressure, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(us_v_be_t20, us_venezuela_oil_pressure, base_extractiveness, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_venezuela_oil_pressure, resource_allocation).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, opec_oil_production).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, us_iran_sanctions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
