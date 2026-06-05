% ============================================================================
% CONSTRAINT STORY: us_taiwan_arms_sales
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_taiwan_arms_sales, []).

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
 *   constraint_id: us_taiwan_arms_sales
 *   human_readable: US Arms Sales Policy toward Taiwan
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The US Arms Sales Policy toward Taiwan is a long-standing geopolitical
 *   constraint aimed at deterring Chinese aggression and maintaining
 *   stability in the region. Codified in the Taiwan Relations Act, this
 *   policy involves the sale of defensive weapons systems to Taiwan. However,
 *   this policy also creates structural tensions between competing interests,
 *   extraction from some parties and coordination for others. The analytical
 *   perspective shows how the overall situation is a tangled rope.
 *
 * KEY AGENTS:
 *   - US Defense Industry: Primary beneficiary (institutional/arbitrage) - profits from arms sales.
 *   - Taiwan Defense Forces: Secondary beneficiary (institutional/constrained) - receives advanced weaponry.
 *   - China Regional Influence: Primary victim (powerless/trapped) - constrained by US support for Taiwan.
 *   - US Taxpayers: Secondary victim (moderate/constrained) - funds the arms sales.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_taiwan_arms_sales, 0.55).
domain_priors:suppression_score(us_taiwan_arms_sales, 0.4).
domain_priors:theater_ratio(us_taiwan_arms_sales, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_taiwan_arms_sales, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_taiwan_arms_sales, tangled_rope).
narrative_ontology:human_readable(us_taiwan_arms_sales, "US Arms Sales Policy toward Taiwan").
narrative_ontology:topic_domain(us_taiwan_arms_sales, "geopolitical").

domain_priors:requires_active_enforcement(us_taiwan_arms_sales).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, us_defense_industry).
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, taiwan_defense_forces).
narrative_ontology:constraint_victim(us_taiwan_arms_sales, china_regional_influence).
narrative_ontology:constraint_victim(us_taiwan_arms_sales, us_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINA (SNARE) — China's regional influence is constrained by US arms sales, limiting its options for asserting control over Taiwan. China is essentially trapped in this scenario, bearing the cost of containment.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: US TAXPAYERS (TANGLED ROPE) — US Taxpayers bear the financial burden of subsidizing arms sales but also benefit from the perceived security and stability provided by the policy. Their exit options are constrained by the bipartisan political consensus on Taiwan's defense.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US DEFENSE INDUSTRY (ROPE) — US defense contractors benefit economically from arms sales to Taiwan, experiencing the constraint as a coordination mechanism that ensures continued revenue and market access. They can 'arbitrage' this situation by lobbying to maintain the policy.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: Taiwan's Defense Forces (Rope) - Benefits from the increased military capabilities and deterrence provided by the arms sales. Although constrained by the US’s selection and timing of arms provisions, the policy overall offers a coordination benefit for Taiwan.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global, civilizational perspective, the policy is a complex mix of coordination (deterrence, regional stability) and extraction (financial costs, potential for escalation). Extraction is likely to increase over time as China's military capabilities grow.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_taiwan_arms_sales_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_taiwan_arms_sales, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_taiwan_arms_sales_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to High. The policy requires significant financial investment from US taxpayers, and China experiences it as a constraint on its regional ambitions. Suppression (0.40): Moderate. The policy suppresses China's options for resolving the Taiwan issue by force, and limits Taiwan's strategic autonomy by making it dependent on US arms. Theater ratio (0.30): Low. The policy is primarily functional, with a relatively small performative element. Most activity is directly aimed at improving Taiwan's defenses and sending a signal to China. 
 *
 * PERSPECTIVAL GAP:
 *   China sees the policy as a Snare, directly impeding its goals. US taxpayers experience it as a Tangled Rope because they bear the cost but also perceive a benefit in terms of national security. The US defense industry views it as a Rope, facilitating its economic goals. Taiwan views it as a Rope for the defense forces and an element of deterrence. The analytical observer sees the situation as a Tangled Rope because it embodies elements of both coordination and extraction, with a tendency towards greater extraction as China's military power increases.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each actor's structural position. China, as the target of containment, experiences maximum extraction. US taxpayers bear the financial costs, resulting in moderate extraction. US defense contractors benefit economically, resulting in negative extraction. Taiwan receives coordinated support in defense, so that party experiences extracted benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_response_threshold,
    'At what point does the level/type of arms sales trigger a disproportionate or destabilizing response from China?',
    'Game-theoretic modeling of escalation dynamics; analysis of historical precedent in similar geopolitical scenarios.',
    'If threshold is low: the policy becomes a snare for regional stability. If threshold is high: the policy remains a tangled rope with a net coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_response_threshold, empirical, 'Arms sales level that triggers Chinese overreaction').

omega_variable(
    taiwan_defense_autonomy,
    'To what extent does reliance on US arms sales undermine Taiwan''s own defense industrial base and strategic autonomy?',
    'Comparative analysis of Taiwan''s defense budget allocation; assessment of indigenous weapons development programs.',
    'If high dependence: Taiwan remains trapped. If Taiwan builds up its independent base: Taiwan will have greater mobile exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taiwan_defense_autonomy, empirical, 'Taiwan''s dependence on US arms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_taiwan_arms_sales, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_t_tr_t0, us_taiwan_arms_sales, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_t_tr_t10, us_taiwan_arms_sales, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_t_tr_t20, us_taiwan_arms_sales, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(us_t_be_t0, us_taiwan_arms_sales, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_t_be_t10, us_taiwan_arms_sales, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(us_t_be_t20, us_taiwan_arms_sales, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_taiwan_arms_sales, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
