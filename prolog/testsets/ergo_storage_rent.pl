% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent, []).

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
 *   constraint_id: ergo_storage_rent
 *   human_readable: Ergo Storage Rent (UTXO Demurrage)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Ergo's storage rent (UTXO demurrage) is a structural constraint that
 *   charges inactive UTXOs a periodic fee to incentivize blockchain state
 *   pruning and encourage monetary velocity. The mechanism extracts value
 *   from long-term holders to subsidize network efficiency for active
 *   participants. Storage rent exhibits the full indexical spectrum: it
 *   appears as coordination mechanism (Rope) to active traders, as temporary
 *   state-management necessity (Scaffold) to scaling-aware developers, as
 *   degraded ritual (Piton) to protocol architects, as mixed
 *   extraction-coordination (Tangled Rope) to moderate inactive holders, as
 *   predatory wealth extraction (Snare) to long-term hodlers, and as an
 *   immutable information economics constraint (Mountain) from a
 *   civilizational-scope analytical view. The perspectival gap is sharp: the
 *   same 0.25 ERG per year per 1000 bytes appears as fair coordination
 *   overhead to one observer and as uncompensated wealth confiscation to
 *   another.
 *
 * KEY AGENTS:
 *   - Long-term hodlers: Primary victim (powerless/trapped) — forced to pay demurrage or lose holdings; no meaningful exit
 *   - Inactive retail investors: Secondary victim (moderate/constrained) — bear extraction cost but with some exit capacity through movement (transactional friction)
 *   - Active traders and merchants: Primary beneficiary (institutional/arbitrage) — experience demurrage as low-cost coordination mechanism; benefit from reduced UTXO bloat and faster full-node sync
 *   - Storage resource maintainers: Beneficiary (institutional/arbitrage) — reduced full-node resource burden from UTXO set pruning
 *   - Protocol developers: Institutional perspective (institutional/arbitrage) — maintain demurrage as governance tool; recognize theatrical justification component
 *   - Layer-2 scaling coalition: Organized agents (organized/constrained) — see storage rent as temporary necessity; building alternatives with sunset path
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing design choice as immutable constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent, 0.38).
domain_priors:suppression_score(ergo_storage_rent, 0.52).
domain_priors:theater_ratio(ergo_storage_rent, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent, extractiveness, 0.38).
narrative_ontology:constraint_metric(ergo_storage_rent, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ergo_storage_rent, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent, "Ergo Storage Rent (UTXO Demurrage)").
narrative_ontology:topic_domain(ergo_storage_rent, "technological/economic").

domain_priors:requires_active_enforcement(ergo_storage_rent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent, active_network_participants).
narrative_ontology:constraint_beneficiary(ergo_storage_rent, storage_resource_maintainers).
narrative_ontology:constraint_victim(ergo_storage_rent, long_term_hodlers).
narrative_ontology:constraint_victim(ergo_storage_rent, dormant_account_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LONG-TERM HOLDER (SNARE) — Cannot exit without loss of wealth. Storage rent extracts value from dormant UTXOs at approximately 0.25 ERG per year per 1000 bytes. Holders who leave coins unspent for 4+ years face mandatory attrition. No alternative storage mechanism; no meaningful exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(ergo_storage_rent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INACTIVE RETAIL INVESTOR (TANGLED ROPE) — Constrained by knowledge barriers and transaction friction; may not understand demurrage mechanism. Benefits from network's reduced blockchain bloat and improved scalability. Extraction is significant but not absolute — can exit by moving coins (albeit at transaction cost), but incentivized to forget or abandon small amounts rather than pay fees to reclaim them.
constraint_indexing:constraint_classification(ergo_storage_rent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ACTIVE EXCHANGE OR MERCHANT (ROPE) — Benefits from demurrage-driven fee structure. Transaction volume keeps UTXOs fresh; storage rent is a minor operating cost. Experiences the constraint as coordination: demurrage incentivizes network-level housekeeping (pruning dead UTXOs), reducing full-node resource requirements and enabling decentralization. Net beneficiary.
constraint_indexing:constraint_classification(ergo_storage_rent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DEVELOPER (PITON) — Storage rent is a governance tool with theoretical elegance (demurrage encourages velocity; reduces UTXO set explosion) but operational effectiveness is degraded. In practice, long-term holders move coins to avoid rent, creating artificial transaction churn — the constraint is maintained through institutional commitment to the model rather than through persistent functional superiority over simple UTXO fees. Theater ratio high because the demurrage mechanism requires constant narrative justification.
constraint_indexing:constraint_classification(ergo_storage_rent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BLOCKCHAIN STATE OPTIMIZATION COALITION (SCAFFOLD) — Organized nodes and developers see storage rent as a temporary solution to UTXO set growth, with eventual sunset as layer-2 scaling (rollups, sidechains) matures. The demurrage mechanism is a coordination device to manage on-chain resource constraints until better solutions exist. As network throughput increases and state efficiency improves, the functional need for demurrage declines — sunset estimated at 5-15 years as scaling solutions mature.
constraint_indexing:constraint_classification(ergo_storage_rent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION ECONOMICS VIEW (MOUNTAIN) — From a universal civilizational perspective, storage rent reflects an immutable constraint of distributed systems: decentralization requires every participant to maintain full state, creating a tragedy of the commons unless usage incentives are aligned. Demurrage is a natural law solution to aligning individual incentives (minimize storage burden) with collective health (UTXO pruning). However, this perspective risks naturalizing a design choice — the constraint derives from Ergo's specific architectural commitment to state transparency and full-node accessibility, not from physics or mathematics.
constraint_indexing:constraint_classification(ergo_storage_rent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_storage_rent, TR),
    TR >= 0.70.

:- end_tests(ergo_storage_rent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Storage rent extracts from dormant holders (~0.25 ERG per year per 1000 bytes, approximately 0.04% annual wealth decay depending on unit density), but the extraction is bounded and proportional to inactivity duration. Not as severe as a lender-of-last-resort extraction mechanism (which would approach 0.60+), but not negligible. The mechanism's purpose includes genuine network coordination (UTXO pruning), which moderates the pure extractiveness component. Suppression (0.52): Moderate-high. Significant barriers to exit include: (1) transaction costs to move coins, (2) knowledge barriers — many holders unaware of demurrage until rent is due, (3) psychological friction — moving coins to avoid rent creates artificial churn and defeats the mechanism's purported efficiency goal. However, exit is not impossible — holders can pay transaction fees to reclaim holdings or move to layer-2. Theater ratio (0.35): Moderate-low. The demurrage mechanism has genuine technical function (incentivizing UTXO pruning, reducing node storage), but protocol narratives often overstate its necessity and elegance. As layer-2 solutions mature, the functional justification weakens faster than the mechanism is retired — the theatrical component increases during sunset phases.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence between institutional beneficiaries and powerless victims. Active traders see Rope (coordination): demurrage incentivizes velocity and pruning, reducing full-node sync time from days to hours. Long-term hodlers see Snare (extraction): their wealth decays at 4% per four-year cycle independent of network use. The scaffold perspective (organizing developers) sees a genuine sunset mechanism — layer-2 solutions will eventually replace on-chain demurrage — but the actual timeline and market adoption path are uncertain. The piton perspective (protocol architects) observes that demurrage is maintained through institutional commitment and narrative justification more than through persistent technical superiority. The analytical observer's mountain risks naturalizing a specific architectural choice (Ergo's commitment to lightweight full nodes and state transparency) as an immutable law of distributed systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (active_network_participants, storage_resource_maintainers) experience low d-values through the derivation chain: they are institutional actors with arbitrage options (can move funds freely; bear minimal storage rent relative to transaction volume). Their d-values map to the beneficiary end of the spectrum (d ≈ 0.05-0.15). Victims (long_term_hodlers) experience high d-values: they are powerless agents with trapped exit options (cannot leave the network without permanent loss; cannot avoid demurrage without transaction cost). Their d-values map to the victim end (d ≈ 0.85-0.95). The sigmoid f(d) amplifies the extraction experienced by victims and dampens it for beneficiaries, producing the perspectival gap: the snare perspective from the trapped agent and the rope perspective from the institutional beneficiary derive from the same structural data (extractiveness 0.38, suppression 0.52) but different d-values.
 *
 * MANDATROPHY ANALYSIS:
 *   Storage rent resolves mandatrophy through explicit structural classification: it is a Tangled Rope that genuinely includes both coordination (UTXO pruning, network efficiency) and extraction (wealth decay for dormant holders). The perspectival gap is not a failure of classification but a feature — different observers legitimately perceive the same mechanism as dominant coordination (Rope), dominant extraction (Snare), or balanced hybrid (Tangled Rope) depending on their structural position. The constraint avoids mandatrophy collapse by: (1) explicitly declaring beneficiaries and victims, (2) showing why network-level efficiency gains are real but unequally distributed, (3) documenting the scaffold sunset path (layer-2 scaling will reduce on-chain demurrage need), (4) acknowledging the piton component (theatrical justification). No single perspective is the 'correct' classification; the presheaf of six perspectives IS the correct answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demurrage_rate_empirical_optimality,
    'Does the 0.25 ERG per year per 1000 bytes demurrage rate optimally balance UTXO pruning against unintended wealth extraction from legitimate long-term holders?',
    'Historical analysis of UTXO set growth and pruning rates pre- and post-demurrage; measurement of forced transaction churn attributable to rent avoidance; economic modeling of fair rent vs. coordination incentive threshold',
    'If rate too high: demurrage functions as wealth extraction (Snare) even from perspectival beneficiaries. If rate too low: UTXO set still explodes and demurrage is theater (Piton). If optimized: true Tangled Rope coordination-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demurrage_rate_empirical_optimality, empirical, 'Whether demurrage rate is optimally calibrated for network health').

omega_variable(
    layer_two_scaling_timeline,
    'At what stage of layer-2 maturity (rollups, sidechains, sharding) does on-chain storage rent become functionally obsolete?',
    'Historical analysis of Ethereum''s state growth vs. layer-2 adoption; technical assessment of when Ergo could safely disable demurrage without state explosion; community consensus on when sunset could be declared',
    'If layer-2 solutions mature before storage rent extraction becomes severe: Scaffold classification confirmed (genuine sunset path). If demurrage persists despite mature alternatives: constraint reclassifies toward Piton (inertial theater) or Snare (wealth extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_two_scaling_timeline, empirical, 'Timeline for functional obsolescence of on-chain storage rent via layer-2 scaling').

omega_variable(
    intentional_dormancy_vs_loss,
    'What fraction of demurrage collection derives from genuinely lost keys vs. intentional long-term holding strategies? Can this distinction be detected via on-chain behavior analysis?',
    'Statistical clustering of UTXO movement patterns; identification of dust accumulation signatures; survey of hodler behavior and motivations; time-series analysis of rent payment patterns',
    'If lost-key fraction is very high (>60%): demurrage functions primarily as lost-coin recycling (coordination, Rope). If low (<20%): demurrage is intentional extraction from active hodlers (Snare). If mixed: validates Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentional_dormancy_vs_loss, empirical, 'Fraction of storage rent from intentional holding vs. lost keys').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergsr_tr_t0, ergo_storage_rent, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ergsr_tr_t3, ergo_storage_rent, theater_ratio, 3, 0.32).
narrative_ontology:measurement(ergsr_tr_t6, ergo_storage_rent, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(ergsr_be_t0, ergo_storage_rent, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ergsr_be_t3, ergo_storage_rent, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(ergsr_be_t6, ergo_storage_rent, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent, resource_allocation).
narrative_ontology:affects_constraint(ergo_storage_rent, utxo_set_explosion).
narrative_ontology:affects_constraint(ergo_storage_rent, full_node_resource_burden).

% DUAL FORMULATION NOTE:
% Storage rent is downstream of UTXO set growth dynamics and node resource constraints. The upstream constraints (utxo_set_explosion, full_node_resource_burden) have their own extractiveness profiles reflecting empirical UTXO growth rates; storage rent as demurrage represents a governance solution attempting to manage those upstream constraints. Decomposition enables separate analysis of whether demurrage is effective resource management (Tangled Rope) or ineffective theater (Piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
