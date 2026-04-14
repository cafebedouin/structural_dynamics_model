% ============================================================================
% CONSTRAINT STORY: decentralized_governance_scalability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralized_governance_scalability, []).

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
 *   constraint_id: decentralized_governance_scalability
 *   human_readable: Decentralized Governance Scalability Constraint
 *   domain: political_economy/governance_technology
 *
 * SUMMARY:
 *   Decentralized governance systems present a fundamental scalability
 *   tension: as protocols grow to billions of users, maintaining genuine
 *   collective decision-making becomes technically and economically
 *   infeasible. The constraint operates across multiple strata simultaneously
 *   — from powerless peripheral users trapped in protocol rules they cannot
 *   influence, to moderate node operators bearing optimization costs, to
 *   institutional beneficiaries (protocol foundations, early token holders)
 *   capturing governance authority through token concentration and technical
 *   expertise gatekeeping. The theater_ratio trajectory (0.25 → 0.58)
 *   reflects the degradation of governance legitimacy as actual
 *   decision-making consolidates while voting rituals persist. This is
 *   neither pure coordination nor pure extraction, but a hybrid (tangled
 *   rope) where genuine censorship-resistance and transparent rule-making
 *   coexist with systematic exclusion of non-wealthy participants. Layer-2
 *   scaling solutions and alternative governance architectures offer
 *   potential exits, but only if they can solve the governance problem
 *   without reproducing the same extraction mechanisms at different scales.
 *
 * KEY AGENTS:
 *   - Peripheral Participants: Primary victims (powerless/trapped) — users with tokens but no governance voice; bear costs of gas fees, technical complexity, unfavorable protocol changes
 *   - Mid-Tier Node Operators: Secondary victims (moderate/constrained) — earn protocol rewards but face increasing participation barriers and diluted governance influence as token concentration rises
 *   - Protocol Foundations & Early Token Holders: Primary beneficiaries (institutional/arbitrage) — control governance through token concentration, treasury funds, and technical authority; capture first-mover rewards and network effects
 *   - Layer-2 Developers & Sidechain Operators: Organized agents (organized/constrained) — building scaling solutions that reduce governance participation costs; potential sunset path if successful
 *   - Core Developer Teams: Institutional gatekeepers (institutional/arbitrage) — retain technical veto power over protocol changes; their autonomy relative to token voting determines whether governance is binding or performative
 *   - Analytical Observer: Cross-position analyst (analytical/analytical) — tracks whether governance paradox (decentralization rhetoric vs centralized control) is endemic to distributed systems or contingent on current design choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralized_governance_scalability, 0.58).
domain_priors:suppression_score(decentralized_governance_scalability, 0.65).
domain_priors:theater_ratio(decentralized_governance_scalability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralized_governance_scalability, extractiveness, 0.58).
narrative_ontology:constraint_metric(decentralized_governance_scalability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(decentralized_governance_scalability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralized_governance_scalability, tangled_rope).
narrative_ontology:human_readable(decentralized_governance_scalability, "Decentralized Governance Scalability Constraint").
narrative_ontology:topic_domain(decentralized_governance_scalability, "political_economy/governance_technology").

domain_priors:requires_active_enforcement(decentralized_governance_scalability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralized_governance_scalability, protocol_designers).
narrative_ontology:constraint_beneficiary(decentralized_governance_scalability, early_token_holders).
narrative_ontology:constraint_beneficiary(decentralized_governance_scalability, technical_governance_councils).
narrative_ontology:constraint_victim(decentralized_governance_scalability, participating_users).
narrative_ontology:constraint_victim(decentralized_governance_scalability, peripheral_network_nodes).
narrative_ontology:constraint_victim(decentralized_governance_scalability, excluded_non_token_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL PARTICIPANT (SNARE) — Users locked into decentralized protocols with no meaningful governance voice. Cannot exit without complete asset abandonment. Voting power concentrates in whale token holders; participation costs (gas fees, technical complexity) extract continuously. Maximum coercion with minimal coordination benefit.
constraint_indexing:constraint_classification(decentralized_governance_scalability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER NODE OPERATOR (TANGLED ROPE) — Genuine coordination benefit: validating transactions, earning protocol rewards. Constrained by technical requirements, capital barriers to competitive hardware. Extraction: governance voice diluted as token concentration increases; protocol changes impose uncompensated costs. Mixed coordination and extraction, with organizational capacity to protest.
constraint_indexing:constraint_classification(decentralized_governance_scalability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROTOCOL FOUNDATION (ROPE) — Core coordination function: establishing technical standards, resolving disputes, managing protocol upgrades. Net beneficiary through foundation treasury, governance authority, ability to arbitrage between competing implementations. Experienced as pure coordination from this vantage.
constraint_indexing:constraint_classification(decentralized_governance_scalability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LAYER-2 EXIT COALITION (SCAFFOLD) — Organized agents (Layer-2 protocols, rollup developers, sidechain operators) see decentralized governance as a temporary coordination bottleneck with a sunset clause. Technical scaling solutions (sharding, rollups, cross-chain bridges) provide exit pathways. Extraction is temporary and declining as alternatives mature. Estimated sunset: 5-10 years as modular blockchain architecture matures.
constraint_indexing:constraint_classification(decentralized_governance_scalability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEMOCRATIC LEGITIMACY THEATER (PITON) — Decentralized governance rituals (token voting, proposal quorum, discussion forums) perform legitimacy while actual decisions concentrate in core developer teams, large holders, and protocol foundations. Theater ratio high (0.58): voting mechanisms exist but are largely confirmatory; substantive governance power has atrophied into technical gatekeeping. Maintained through institutional inertia despite degraded function.
constraint_indexing:constraint_classification(decentralized_governance_scalability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, decentralized governance has genuine coordination functions (censorship resistance, transparent rule-making) alongside structural extraction (token concentration, participation barriers, technical expertise gatekeeping). The constraint is neither pure mechanism nor pure coercion, but hybrid with asymmetric distribution of benefits and costs across participant strata.
constraint_indexing:constraint_classification(decentralized_governance_scalability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralized_governance_scalability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralized_governance_scalability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralized_governance_scalability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralized_governance_scalability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralized_governance_scalability, TR),
    TR >= 0.70.

:- end_tests(decentralized_governance_scalability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Initial value (0.35) reflects genuine coordination benefits of early protocols: censorship resistance, transparent rules, distributed validation. Rising to 0.58 reflects increasing extraction as token concentration occurs, governance participation costs (gas fees, technical knowledge) rise, and decision-making consolidates. The value is not extreme (0.72+) because some coordination function persists — protocols do maintain distributed validation and transparent rule-making. Suppression (0.65): Substantial barriers exist at multiple levels: technical complexity of governance participation, capital requirements for meaningful token holdings, professional expertise barriers to proposal development, and structural entrenchment of core developers. Participation costs (transaction fees, computational requirements) extract continuously. Theater ratio (0.58): Moderate-high. Voting mechanisms perform legitimacy but actual power concentrates in developers, foundations, and whale holders. Token voting is confirmatory rather than binding for major architectural decisions. The ratio rises over time as complexity increases and participation becomes more ritual than functional.
 *
 * PERSPECTIVAL GAP:
 *   The classification gap reveals the core structural paradox: the same protocol rules are coordination mechanism (rope) from the foundation perspective and extraction mechanism (snare) from the powerless peripheral perspective. The tangled rope classification at the analytical level captures this hybrid — both functions are simultaneously true. The perspectival gap is diagnostic: if all perspectives produced the same type, the constraint would be pure mechanism or pure coercion. The gap indicates a genuine hybrid that requires tangled rope classification and active enforcement to maintain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derived from structural positions: Peripheral powerless/trapped participants experience high d (0.90+) — full extraction target. Mid-tier constrained operators experience moderate d (0.55-0.65) — mixed beneficiary/victim status. Institutional beneficiaries with arbitrage options experience low d (0.10-0.20) — full beneficiary status. The analytical observer experiences moderate d (0.72) from the observation position that sees both functions equally weighted. Token concentration and governance authority flow from beneficiaries to victims, creating asymmetric extraction despite coordination mechanisms that benefit all agents marginally. Protocol foundations derive extraordinary d reduction (0.05) from arbitrage capacity — if unhappy with governance, they can fork and maintain treasury and technical authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'decentralized governance' is a misnomer for a system with real coordination functions (censorship resistance, transparent rules) and real extraction mechanisms (token concentration, participation barriers, expertise gatekeeping). The mandatrophy — 'is this cooperation or coercion?' — is false dichotomy. It is both. The constraint is neither falsely branded extraction (like a snare called 'coordination') nor falsely branded coordination (like a rope called 'extraction'). The tangled rope type is appropriate because both functions are genuine and both are essential to the system's operation. The extraction exists not as a bug but as a feature that concentrates decision-making authority to enable protocol upgrades and dispute resolution. Eliminating the extraction would eliminate the coordination function (no authority to enforce rules). Eliminating the coordination would eliminate the extraction (no system to extract from).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    token_concentration_endpoint,
    'At what token concentration threshold does decentralized governance collapse into de facto oligarchy?',
    'Empirical measurement of token Gini coefficient across protocols; correlation with governance decision reversals and participation rates as concentration increases',
    'If threshold < 40% (top 10 holders): current structures already oligarchic, extraction dominant over coordination. If threshold > 70%: token concentration alone insufficient to determine governance degradation; other factors (technical complexity, participation costs) dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(token_concentration_endpoint, empirical, 'Token concentration threshold for governance oligarchy').

omega_variable(
    participation_cost_scalability,
    'Do scaling solutions (Layer-2, sharding) actually reduce governance participation barriers or merely concentrate barriers at higher layers?',
    'Comparative analysis of governance participation rates on Layer-1 vs Layer-2 protocols; cost of governance interaction (gas fees, technical complexity) across layers; identity-based exclusion patterns',
    'If barriers truly reduce: scaffold perspective is correct, sunset is real, constraint degradation is structural. If barriers shift upward: layer-2 solutions reproduce same extraction at different scale (constraint family with ε > 0.60 for each layer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_cost_scalability, empirical, 'Whether scaling solutions reduce governance participation barriers').

omega_variable(
    core_developer_autonomy,
    'Can core developer teams implement protocol changes over token-holder objections, or is token voting genuinely binding?',
    'Historical analysis of rejected proposals and protocol forks; instances where token votes were overridden or ignored; coordination costs of fork threats vs implementation compliance',
    'If developers are autonomous: token voting is theater (piton classification strengthened). If token voting is binding: coordination function is real (tangled_rope justified). If conditional (dependent on proposal type): different governance types have different structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(core_developer_autonomy, empirical, 'Whether token voting is binding or performative relative to core developers').

omega_variable(
    non_token_holder_governance_voice,
    'Do non-token-holding users (who may have greater exposure to governance outcomes) have meaningful mechanisms to influence protocol decisions?',
    'Inventory of governance participation channels available to non-holders; measurement of decision influence from non-holder inputs; examples of non-holder concerns that changed protocol direction vs were ignored',
    'If meaningful mechanisms exist: snare classification is overstated, some agents are constrained rather than trapped. If no mechanisms exist: extraction from non-holders is near-total, snare classification holds for majority of users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_token_holder_governance_voice, empirical, 'Governance voice availability to non-token-holding users').

omega_variable(
    scalability_solution_governance_inheritance,
    'Do Layer-2 and scaling solutions inherit governance extraction or establish genuinely different governance structures?',
    'Structural comparison of governance mechanisms across Layer-1, Layer-2, sidechains; analysis of whether scaling solutions introduce new extraction mechanisms (different token holders, new participation barriers)',
    'If governance inherited: constraint family multiplies (separate story per layer/solution, each with ε ≥ 0.50). If structures differ: scaffold sunset is real (solutions offer genuine exit). If solutions introduce new extraction: sunset is false, constraint mutates rather than resolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_solution_governance_inheritance, empirical, 'Whether scaling solutions replicate or escape governance extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralized_governance_scalability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgov_tr_t0, decentralized_governance_scalability, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dgov_tr_t3, decentralized_governance_scalability, theater_ratio, 3, 0.42).
narrative_ontology:measurement(dgov_tr_t6, decentralized_governance_scalability, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dgov_be_t0, decentralized_governance_scalability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgov_be_t3, decentralized_governance_scalability, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(dgov_be_t6, decentralized_governance_scalability, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralized_governance_scalability, enforcement_mechanism).
narrative_ontology:affects_constraint(decentralized_governance_scalability, cryptocurrency_consensus_scalability).
narrative_ontology:affects_constraint(decentralized_governance_scalability, blockchain_sybil_attack_resistance).
narrative_ontology:affects_constraint(decentralized_governance_scalability, wealth_inequality_amplification).

% DUAL FORMULATION NOTE:
% Decentralized governance scalability decomposes into at least three structurally distinct constraints: (1) technical consensus scalability (ε=0.15, rope — pure technical coordination), (2) governance mechanism scalability (ε=0.58, tangled_rope — this story), and (3) wealth concentration via token accumulation (ε=0.72, snare — downstream extraction). Each has different base metrics and different exit pathways. Governance scalability is downstream of consensus scalability (cannot solve governance without solving consensus) and upstream of wealth concentration (governance mechanisms determine how tokens concentrate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralized_governance_scalability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
