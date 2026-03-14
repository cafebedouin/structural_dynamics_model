% ============================================================================
% CONSTRAINT STORY: brazilian_party_system_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazilian_party_system_fragmentation, []).

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
 *   constraint_id: brazilian_party_system_fragmentation
 *   human_readable: Brazilian Party System Fragmentation and Coalition Governance
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Brazil's party system fragmentation — characterized by a large number of
 *   small parties with significant legislative representation — creates a
 *   structural constraint where executive governance depends on assembling
 *   coalitions from parties whose interests often diverge sharply from the
 *   executive's policy agenda or from each other. This constraint exhibits
 *   the characteristics of a Tangled Rope: genuine coordination function (the
 *   executive needs legislative support for governance) combined with
 *   asymmetric extraction (small parties extract ministerial positions,
 *   budget allocations, and legislative veto power disproportionate to their
 *   electoral mandate). The fragmentation has increased substantially since
 *   redemocratization in 1985, reaching peaks of 30+ registered parties and
 *   effective fragmentation indexes among the world's highest. Theater ratio
 *   (0.65) reflects that much coalition formation ritual is performative —
 *   parties announce coalition membership, receive ministerial posts, and
 *   then vote against their coalition's legislation on specific bills,
 *   indicating that the coalition is partly a facade for position
 *   distribution rather than genuine policy coordination. Extractiveness has
 *   risen from 0.35 (1995) to 0.58 (2015) as small parties have learned to
 *   extract greater value from coalitions.
 *
 * KEY AGENTS:
 *   - Voters: Powerless/trapped — cannot exit the system; their preferred party may be co-opted into contradictory coalitions; representation becomes opaque
 *   - Executive Coalition Managers: Institutional/arbitrage — benefit from flexibility to distribute ministerial positions and budgetary leverage; solve coordination problem through spoils distribution
 *   - Small Party Leadership: Powerful/mobile — extract value through ministerial positions, budget allocations, veto threats; can join different coalitions, creating leverage asymmetries
 *   - Large Party Establishment: Organized/constrained — benefit from legislative leverage but constrained by need to distribute spoils; face party discipline problems when deputies defect to coalition-backed positions
 *   - Electoral System and Democratic Ritual: Institutional/arbitrage — maintains proportional representation system through inertia and ideological commitment despite serving as theater for coalition play
 *   - Institutional Reform Movements: Organized/constrained — view fragmentation as temporary problem solvable through electoral threshold increases or electoral system changes; constrained by incumbent party resistance
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent institutional design (proportional representation) as immutable feature of large diverse democracies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazilian_party_system_fragmentation, 0.58).
domain_priors:suppression_score(brazilian_party_system_fragmentation, 0.52).
domain_priors:theater_ratio(brazilian_party_system_fragmentation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazilian_party_system_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(brazilian_party_system_fragmentation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(brazilian_party_system_fragmentation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazilian_party_system_fragmentation, tangled_rope).
narrative_ontology:human_readable(brazilian_party_system_fragmentation, "Brazilian Party System Fragmentation and Coalition Governance").
narrative_ontology:topic_domain(brazilian_party_system_fragmentation, "political/institutional").

domain_priors:requires_active_enforcement(brazilian_party_system_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazilian_party_system_fragmentation, executive_branch_coalition_managers).
narrative_ontology:constraint_beneficiary(brazilian_party_system_fragmentation, smaller_parties_leverage_positions).
narrative_ontology:constraint_victim(brazilian_party_system_fragmentation, legislative_coherence).
narrative_ontology:constraint_victim(brazilian_party_system_fragmentation, policy_consistency).
narrative_ontology:constraint_victim(brazilian_party_system_fragmentation, voter_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOTER (SNARE) — Individual voters cannot exit the fragmentation constraint. Their preferred party may be co-opted into contradictory coalitions; legislative representation becomes opaque as small parties trade support for ministerial positions. Voters bear full cost of incoherent governance while having no mechanisms to compel party fidelity. No exit option.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXECUTIVE COALITION MANAGER (ROPE) — The executive (president and advisors) experiences fragmentation as a coordination mechanism. Assembling winning coalitions from small parties is difficult but enables stable governance. The constraint solves the problem of how a minority president governs in a fragmented legislature. Benefits from flexibility to distribute ministerial positions and budgetary leverage.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL PARTY LEADERSHIP (TANGLED ROPE) — Small parties benefit from fragmentation (ministerial positions, budget allocations, veto power over legislation) while also being constrained by it. They extract value by threatening to withdraw from coalitions; they are extracted from when coalitions reorganize. High mobility (can join different coalitions) but also asymmetric gains relative to large parties.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE PARTY ESTABLISHMENT (TANGLED ROPE) — Large parties benefit from fragmentation in legislative leverage (can cobble together majorities) but are constrained by the need to distribute spoils to coalition partners. They face high costs of party discipline maintenance when their deputies defect to coalition-backed positions. Constrained exit because leaving coalition play means legislative isolation.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL SYSTEM AND DEMOCRATIC RITUAL (PITON) — The proportional representation system that generates fragmentation is maintained as a symbol of democratic participation despite serving as a theater for coalition play. The system persists through institutional inertia and ideological commitment to proportionalism rather than functional legitimacy. Electoral cycles and party registration rituals are performative, as coalition composition changes mid-term independent of electoral mandates.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL REFORM MOVEMENTS (SCAFFOLD) — Civil society organizations and constitutional reformers view fragmentation as a temporary problem solvable through threshold rules, closed-list proportionalism, or other institutional changes. These movements see a sunset path: if institutional design reforms (electoral threshold increases, party merger incentives) take hold, fragmentation would decline. However, implementation is constrained by incumbent parties benefiting from current rules.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, fragmentation appears as an immutable property of large diverse democracies: proportional representation mathematically produces multipartitism; coalition building is inherent to parliamentary governance. This perspective risks naturalizing what is actually a contingent institutional design choice. The engine identifies this as a false summit — the base properties show active enforcement and coordination asymmetries inconsistent with natural law.
constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazilian_party_system_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazilian_party_system_fragmentation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazilian_party_system_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazilian_party_system_fragmentation, TR),
    TR >= 0.70.

:- end_tests(brazilian_party_system_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from voters (who lose representation fidelity) and legislative coherence (policy becomes incoherent as coalition partners extract contradictory positions). However, extraction is not maximal because the executive coalition manager benefits from functional coordination — they actually need to govern and the coalition mechanism enables this. The rising trajectory (0.35 to 0.58 over 20 years) reflects that small parties have learned to extract more systematically as fragmentation has increased and coalition dependency has deepened. Suppression (0.52): Moderate-high. Significant barriers include: (1) institutional lock-in through party registration and electoral system rules that favor proliferation; (2) voter information deficits about coalition formation mechanics; (3) lack of mechanisms to enforce campaign coalition promises; (4) constitutional constraints on coalition modification (changing coalitions mid-term is constitutionally permissible). However, suppression is not total because voters retain electoral choice (albeit constrained) and parties retain ability to propose institutional reforms. Theater ratio (0.65): Moderate-high. Coalition formation includes genuine democratic ritual (coalition agreements are publicly signed, parties claim policy coherence) but is increasingly performative (parties vote against coalition positions, distribution of ministries follows formulaic spoils rules unrelated to policy agreement, coalition composition changes mid-term without electoral authorization).
 *
 * PERSPECTIVAL GAP:
 *   The gap between voter experience (Snare) and executive manager experience (Rope) is maximal — the same constraint that solves the executive's coordination problem extracts from voters through representation loss and governance incoherence. This gap is diagnostic: the constraint's claimed coordination function (enabling executive governance in fragmented legislatures) is genuine but is entangled with extraction (spoils distribution concentrates benefits in small parties and executive, costs dispersed to voters). The small/large party perspectives both show Tangled Rope, confirming that the constraint genuinely coordinates while also extracting from multiple agents at different intensities.
 *
 * DIRECTIONALITY LOGIC:
 *   Small parties experience high extraction value through ministerial positions, budget allocations, and veto power disproportionate to their electoral mandate. However, this 'extraction' is from the executive's budget and the large parties' legislative space, not from society as a whole. Voters experience the extraction as loss of representation and governance coherence. The derivation chain: voters (powerless + trapped exit) → high d → high f(d) → victims classification; small parties (powerful + mobile exit) → moderate d → moderate f(d) → beneficiary classification; executive (institutional + arbitrage) → low d → negative f(d) → beneficiary classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification resolves mandatrophy by showing that fragmentation serves both coordination and extraction functions simultaneously. The coordination function (enabling executive governance) is genuine — the executive cannot govern without coalition assembly, and the constraint mechanism works by distributing ministerial and budgetary resources to coalition partners. The extraction function (spoils distribution) is also genuine — small parties extract value disproportionate to their electoral mandate. The mandatrophy is resolved by documenting both functions explicitly in the beneficiary/victim declarations and in the perspectives. The constraint is not mislabeled as pure coordination (Rope) because asymmetric extraction is clearly documented; it is not mislabeled as pure extraction (Snare) because genuine governance coordination is occurring. The Piton classification for the electoral system reveals that democratic ritual around coalition formation is increasingly performative — the ritual legitimates coalition play but does not constrain it. The Scaffold classification for reform movements is realistic: institutional changes could reduce fragmentation, but implementation is constrained by incumbent party resistance and ideological commitment to proportionalism. The false summit identification reveals that naturalizing fragmentation as immutable ignores the contingency of proportional representation and the possibility of alternative institutional designs (e.g., majority voting, closed-list systems, electoral thresholds).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_optimization_level,
    'Is party system fragmentation at its equilibrium level or is it being artificially maintained above equilibrium through institutional rules that could be changed?',
    'Comparative analysis of fragmentation levels under different electoral system parameters; analysis of merger/dissolution barriers; simulation of alternative institutional designs',
    'If at equilibrium: fragmentation is structural-economic outcome (higher extractiveness, more mountain-like). If artificially maintained: fragmentation is choice-dependent system (lower extractiveness, more rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_optimization_level, empirical, 'Whether fragmentation is equilibrium outcome or institutionally maintained').

omega_variable(
    coalition_stability_mechanism,
    'What mechanism prevents permanent coalition collapse? Is it ideological coherence, material incentives, institutional lock-in, or fear of electoral punishment?',
    'Analysis of coalition duration data; interviews with coalition members on exit barriers; correlation between coalition stability and regime legitimacy',
    'If material incentives only: suppression is low, exit options are better than trapped (reclassify snare to constrained). If institutional lock-in: suppression remains high, snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_mechanism, empirical, 'Primary mechanism maintaining coalition stability').

omega_variable(
    voter_awareness_and_betrayal,
    'Do voters understand the coalition-formation process sufficiently to punish parties for post-election coalition changes, or is coalition betrayal invisible to voter accountability?',
    'Voter surveys on coalition awareness; analysis of electoral punishment for parties that changed coalition allegiances between elections; comparison with voter expectations',
    'If high awareness: extraction is constrained by electoral accountability (lower suppression). If low awareness: extraction is hidden from accountability mechanism (higher effective suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voter_awareness_and_betrayal, empirical, 'Voter awareness of coalition formation dynamics').

omega_variable(
    extractive_vs_coordination_theater,
    'Is the high theater ratio (0.65) genuine democratic ritual or cover for extraction? Do coalition agreements produce actual policy coherence or are they facades for position distribution?',
    'Analysis of coalition agreement compliance; comparison of policy outcomes under coalition governments vs single-party governments; measurement of legislative coherence variance',
    'If genuine coordination theater: theater ratio reflects real democratic engagement (scaffold-supporting). If extractive cover: theater ratio masks asymmetric spoils distribution (snare-supporting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_vs_coordination_theater, empirical, 'Whether theater ratio represents genuine democratic coordination or extraction cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazilian_party_system_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bpsf_tr_t0, brazilian_party_system_fragmentation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bpsf_tr_t10, brazilian_party_system_fragmentation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(bpsf_tr_t20, brazilian_party_system_fragmentation, theater_ratio, 20, 0.65).
narrative_ontology:measurement(bpsf_tr_t5, brazilian_party_system_fragmentation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(bpsf_tr_t15, brazilian_party_system_fragmentation, theater_ratio, 15, 0.61).

% Extraction over time
narrative_ontology:measurement(bpsf_be_t0, brazilian_party_system_fragmentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bpsf_be_t10, brazilian_party_system_fragmentation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bpsf_be_t20, brazilian_party_system_fragmentation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(bpsf_be_t5, brazilian_party_system_fragmentation, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(bpsf_be_t15, brazilian_party_system_fragmentation, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazilian_party_system_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(brazilian_party_system_fragmentation, brazilian_legislative_gridlock).
narrative_ontology:affects_constraint(brazilian_party_system_fragmentation, budget_reallocation_cycles).
narrative_ontology:affects_constraint(brazilian_party_system_fragmentation, policy_volatility_via_coalition_turnover).

% DUAL FORMULATION NOTE:
% Party fragmentation is decomposed into three related constraints: (1) fragmentation itself (this story) — the structural incentive to maintain small parties; (2) legislative gridlock — the downstream consequence of coalition instability; (3) budget reallocation cycles — the mechanism through which small parties extract value. Each has its own ε and perspectives. This story is upstream of the gridlock and budget stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
