% ============================================================================
% CONSTRAINT STORY: jurisdictional_exit_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisdictional_exit_capacity, []).

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
 *   constraint_id: jurisdictional_exit_capacity
 *   human_readable: Jurisdictional Exit Capacity Constraint
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Jurisdictional exit capacity is the structural ability of agents to
 *   relocate, transfer assets, and credential across political boundaries.
 *   The constraint operates through legal mechanisms (passport denial, visa
 *   restriction, citizenship revocation, exit taxation), economic mechanisms
 *   (credential non-recognition, professional licensing friction, capital
 *   controls, wealth confiscation), and social mechanisms (family separation
 *   penalties, social network loss, identity dissolution). The constraint
 *   exhibits different classification types depending on the observer's
 *   structural position: trapped residents without exit options see it as a
 *   Snare; mobile professionals with high-cost but possible exit see it as
 *   Tangled Rope; destination states seeking selective immigration see
 *   coordination benefits (Rope); wealthy agents with arbitrage options
 *   extract value from others' immobility (Tangled Rope from above); the
 *   incumbent state apparatus sees its own degraded control mechanism
 *   (Piton); and analytical observers risk naturalizing jurisdictional
 *   boundaries as immutable (false Mountain). The extractiveness score (0.58)
 *   reflects that exit capacity is asymmetrically distributed by wealth,
 *   credentials, and power, creating a stratified constraint where the burden
 *   falls on immobile agents while mobile agents arbitrage the difference.
 *   The suppression score (0.72) reflects multiple binding mechanisms: legal
 *   restrictions, credential friction, capital controls, and internalized
 *   identity-lock preventing even mobile agents from exercising exit options.
 *   Theater_ratio (0.48) is moderate because many exit barriers are
 *   performative rather than actually binding — visa queues, licensing
 *   bureaucracy, and residence permitting are often circumvented through
 *   corruption, credential recognition fraud, or capital smuggling, yet the
 *   apparatus maintains the theatrical machinery.
 *
 * KEY AGENTS:
 *   - Immobilized Residents: Primary victims (powerless/trapped) — lack liquid capital, portable credentials, or political connections; cannot exit without catastrophic loss. Bear full extraction cost.
 *   - Mobile Professionals: Secondary victims (moderate/constrained) — possess portable credentials but face high exit costs; benefit from jurisdiction's professional infrastructure; lose benefits upon exit.
 *   - High-Net-Worth Individuals: Secondary beneficiaries (powerful/arbitrage) — can arbitrage exit capacity through capital, multiple residencies, credential-agnostic opportunities; extract value from lower-income residents' immobility.
 *   - Incumbent State Apparatus: Primary beneficiary (institutional/constrained) — extracts taxation, labor supply, capital retention through exit restriction; maintains control mechanisms through inertia.
 *   - Exit Destination States: Institutional actors (institutional/arbitrage) — benefit from selective immigration (cream-skimming high-value agents); coordinate labor market equilibration through credential reciprocity.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent political arrangements as immutable laws of geography/sovereignty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisdictional_exit_capacity, 0.58).
domain_priors:suppression_score(jurisdictional_exit_capacity, 0.72).
domain_priors:theater_ratio(jurisdictional_exit_capacity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisdictional_exit_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisdictional_exit_capacity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisdictional_exit_capacity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisdictional_exit_capacity, tangled_rope).
narrative_ontology:human_readable(jurisdictional_exit_capacity, "Jurisdictional Exit Capacity Constraint").
narrative_ontology:topic_domain(jurisdictional_exit_capacity, "political_economy/governance").

domain_priors:requires_active_enforcement(jurisdictional_exit_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisdictional_exit_capacity, incumbent_state_apparatus).
narrative_ontology:constraint_beneficiary(jurisdictional_exit_capacity, capital_immobilized_within_jurisdiction).
narrative_ontology:constraint_victim(jurisdictional_exit_capacity, mobile_agents_seeking_exit).
narrative_ontology:constraint_victim(jurisdictional_exit_capacity, exit_destination_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILIZED RESIDENT (SNARE) — Residents without liquid capital or portable credentials face material barriers: passport denial, visa restrictions, wealth confiscation threats, family separation penalties, restrictions on skill transfer recognition. Exit is materially blocked. The constraint appears as an absolute immutability from this perspective — the resident cannot leave without catastrophic loss.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MOBILE PROFESSIONAL (TANGLED ROPE) — Professionals with portable credentials (doctors, engineers, technologists) face high but surmountable exit costs: credential recognition delays, licensing friction, relocation expenses, reputation rebuilding in new market. They benefit from the jurisdiction's professional infrastructure and network effects while losing those benefits upon exit. Mixed coordination and extraction.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXIT DESTINATION STATE (ROPE) — Destination jurisdictions benefit from selective immigration (cream-skimming high-value agents) while maintaining lower-cost coordination through credential reciprocity agreements and bilateral trade. The constraint appears as a coordination mechanism: exit capacity enables labor market equilibration and specialization.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-NET-WORTH INDIVIDUAL (TANGLED ROPE) — Wealthy agents with liquid capital can arbitrage exit capacity: purchasing residence permits, establishing offshore holdings, maintaining multiple jurisdictions. They benefit from the incumbent state's infrastructure and stability while paying exit costs below those of less-capitalized agents. Asymmetric extraction: they extract arbitrage value from others' immobility.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT STATE APPARATUS (PITON) — The state maintains exit restrictions through citizenship revocation threats, exit taxation, capital controls, and passport denial — mechanisms that persist through institutional inertia long after their functional justification has eroded. Theater_ratio is high: the apparatus performs demographic control but actual exit prevention relies on coordination failures and information asymmetries rather than force.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some minimum exit friction is inherent: moving across jurisdictions requires processing identity claims, verifying credentials, and establishing residency. Transaction costs are unavoidable. However, the structural data (suppression 0.72, extractiveness 0.58) reveals that the observed constraint vastly exceeds what friction alone produces — the engine will detect this as false naturalization.
constraint_indexing:constraint_classification(jurisdictional_exit_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisdictional_exit_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisdictional_exit_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisdictional_exit_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisdictional_exit_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisdictional_exit_capacity, TR),
    TR >= 0.70.

:- end_tests(jurisdictional_exit_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The incumbent state captures labor supply (preventing exit during productivity peak), capital retention (through capital controls and wealth taxation), and fiscal revenue (exit taxation, credential licensing fees). Beneficiaries of exit restriction include the incumbent apparatus and immobilized capital interests. The extraction is substantial because exit barriers compound: a resident must overcome legal, economic, and social barriers simultaneously. However, extractiveness is not maximal (snare-level 0.66+) because some exit is possible for all agents at varying costs — the constraint is not a total prohibition. Suppression (0.72): Multiple binding mechanisms operate: legal passport control, visa restrictions, credential non-recognition by destination markets, capital controls, family-separation penalties, and internalized identity-lock. The suppression is high because the apparatus maintains machinery at multiple layers and agents often face coordination failures (other jurisdictions' unwillingness to reciprocate credential transfers, destination labor market barriers). Theater_ratio (0.48): Moderate. Exit restrictions are partially theatrical (visa queues, residence permitting bureaucracies can be circumvented through corruption, credential fraud, capital smuggling) but also partially functional (some residents genuinely cannot overcome barriers, and state does maintain deportation machinery). The theater has remained stable because the apparatus does not need to make restrictions actually binding — the perception of restriction plus coordination barriers produces adequate immobilization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the constraint that traps some agents enriches others. Trapped residents see pure extraction (Snare). Wealthy arbitrageurs see pure coordination (Rope or Rope-adjacent). Destination states see labor-market coordination gains. The incumbent apparatus sees its own degradation. The analytical observer sees immutable law. No single type captures the constraint because the constraint's function is to sort agents by exit capacity — those with high capacity experience it as minor friction (Rope); those with low capacity experience it as total entrapment (Snare). The constraint IS the mechanism that makes exit stratified.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim declarations and power atoms. Immobilized residents (powerless/trapped) have d ≈ 0.95, experiencing maximum extraction. Mobile professionals (moderate/constrained) have d ≈ 0.70, experiencing high but not maximal extraction due to exit options. High-net-worth individuals (powerful/arbitrage) have d ≈ 0.20, experiencing low extraction or arbitrage value extraction from others. Incumbent apparatus (institutional/constrained exit from enforcement machinery) has d ≈ 0.35, benefiting from control but constrained by coordination failures and circumvention. Exit destination states (institutional/arbitrage) have d ≈ 0.10, net beneficiary from selective immigration. The analytical observer (analytical/analytical) has d ≈ 0.72, experiencing constraint through cognitive difficulty of distinguishing inherent limits from contingent institutions. The sigmoid f(d) transforms these d values into experienced chi, showing why trapped agents experience maximum effective extraction while arbitrage agents experience negative effective extraction (they benefit from the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED by recognizing that the constraint's coordination function (sorting agents by exit capacity, equilibrating labor markets across jurisdictions) exists simultaneously with its extraction function (immobilizing low-capacity agents, capturing their productivity, retaining capital). The Tangled Rope classification captures this: beneficiaries include destination states (who coordinate labor markets), incumbent states (who retain capital), and wealthy agents (who arbitrage exit capacity). Victims include immobilized residents (who cannot exit) and exit-destination states (who absorb fiscal costs of integration). The constraint is neither pure coordination nor pure extraction — it is hybrid, and the tangled rope classification prevents mislabeling asymmetric stratification as either natural law (false mountain) or pure market coordination (false rope). The machinery that enables beneficial exit for some agents is the exact machinery that prevents it for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_allocation,
    'Are measured exit barriers (credential delays, visa costs, relocation expenses) coordination costs or extractive overhead?',
    'Comparative analysis: jurisdictions with efficient credential reciprocity (Canada-UK, EU internal market) vs high-friction regimes (non-reciprocal licensing, visa queues). Measure: exit cost as percentage of average income and relationship to legitimate processing requirements.',
    'If reciprocal-market exit costs represent true minimum: constraint is Rope with elevated theater. If non-reciprocal costs exceed processing requirements by >50%: constraint is Snare with intentional friction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_allocation, empirical, 'Whether exit costs are legitimate coordination expenses or extractive padding').

omega_variable(
    immobility_mechanism_ambiguity,
    'Is measured suppression structural (legal barriers) or internalized (cognitive lock, identity fusion with jurisdiction)?',
    'Post-exit trajectory analysis: do constraints on agents'' behavior, identity claims, and risk perception persist after legal barriers are removed? Comparison of emigrants'' behavior immediately post-exit vs post-acculturation.',
    'If structural: suppression should be rated by barrier severity alone. If internalized: effective suppression is higher than formal restrictions suggest; identity-locked exit option may apply to some residents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immobility_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in exit barriers').

omega_variable(
    jurisdictional_stratification,
    'Does exit capacity variation by wealth level (wealthy exit freely, poor exit is trapped) constitute asymmetric extraction or inevitable economics of mobility?',
    'Policy counterfactual: jurisdictions that subsidize exit costs for lower-income groups (e.g., credential recognition programs, relocation assistance) show measurability of exit-cost reduction. Comparison of exit rates before/after subsidy.',
    'If subsidy-responsive: constraint is partially extractive and remediable. If exit rates unchanged despite subsidy: constraint is structural/inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_stratification, empirical, 'Whether exit stratification by wealth is asymmetric extraction or structural economics').

omega_variable(
    network_effects_necessity,
    'To what extent is the ''cost'' of exit (leaving behind professional networks, social capital, credentials) a genuine coordination loss vs a negotiated scarcity that the incumbent state manufactures?',
    'Historical analysis: states that maintained high exit capacity (Switzerland, Singapore) show how rapid credential transfers and network integration reduce network-loss costs. Measurement: speed of professional integration for similar-skill emigrants.',
    'If network losses are inevitable: exit barriers reflect genuine coordination value. If network losses can be engineered away: what appears as ''loss'' is extractive gatekeeping by destination states and incumbent state collusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_necessity, empirical, 'Whether network-loss costs reflect genuine coordination or manufactured scarcity').

omega_variable(
    capital_immobility_intentionality,
    'Is capital immobilization within jurisdictions a side effect of financial infrastructure, or is it deliberately maintained extraction mechanism?',
    'Policy analysis: jurisdictions with high capital mobility (capital gains tax harmonization, treaty networks, transparent beneficial ownership) show whether restrictions are choice or constraint. Measurement: actual capital flow vs legal restrictions.',
    'If restrictions bind: capital immobility is extraction mechanism and contributes to suppression score. If capital flows despite restrictions: theater_ratio should increase (rules are performed but circumvented).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_immobility_intentionality, empirical, 'Whether capital immobility is incidental or deliberately maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisdictional_exit_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jec_tr_t0, jurisdictional_exit_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jec_tr_t5, jurisdictional_exit_capacity, theater_ratio, 5, 0.41).
narrative_ontology:measurement(jec_tr_t10, jurisdictional_exit_capacity, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(jec_be_t0, jurisdictional_exit_capacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jec_be_t5, jurisdictional_exit_capacity, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(jec_be_t10, jurisdictional_exit_capacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisdictional_exit_capacity, resource_allocation).
narrative_ontology:affects_constraint(jurisdictional_exit_capacity, capital_flight_restriction).
narrative_ontology:affects_constraint(jurisdictional_exit_capacity, credential_recognition_friction).
narrative_ontology:affects_constraint(jurisdictional_exit_capacity, citizenship_revocation).
narrative_ontology:affects_constraint(jurisdictional_exit_capacity, exit_taxation).

% DUAL FORMULATION NOTE:
% Jurisdictional exit capacity is the upstream constraint affecting all downstream constraints on capital immobility, credential transfer, and citizenship mechanics. This story treats exit capacity as a unified structural phenomenon; downstream stories decompose specific mechanisms (capital controls, licensing friction, revocation thresholds) with their own ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisdictional_exit_capacity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
