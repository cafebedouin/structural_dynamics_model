% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__store_of_value_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Store-of-Value Reading: Decentralization and Full-Node Verifiability as Binding Constraints
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the store-of-value reading of the
 *   Bitcoin whitepaper's purpose kernel. The reading interprets the
 *   whitepaper's design — 1MB block size cap, ten-minute block time,
 *   proof-of-work consensus, full-node verifiability requirements — as
 *   binding commitments to decentralization and censorship resistance, with
 *   payment use subordinated to preserving these properties. Under this
 *   reading, the constraint is that on-chain capacity must remain limited to
 *   enforce the decentralization and verification guarantees that define
 *   Bitcoin's value proposition as a neutral, uncensorable store of value.
 *   The alternative electronic-cash reading (not generated here) interprets
 *   the same whitepaper as optimizing primarily for peer-to-peer cash
 *   transactions, with decentralization as a means to that end rather than an
 *   end in itself. The two readings invert the priority hierarchy:
 *   store-of-value subordinates capacity to decentralization; electronic-cash
 *   subordinates decentralization costs to transaction throughput. This is
 *   not a mere difference of opinion — it is a structural difference in the
 *   kernel's interpretation that produces different victim/beneficiary sets,
 *   different extractiveness values, and different classification outcomes
 *   from the same perspectives.
 *
 * KEY AGENTS:
 *   - Long-term holders (institutional/arbitrage): Primary beneficiaries — constraint enforces scarcity and justifies premium valuations; no friction in this use case
 *   - Mining and node infrastructure (organized/constrained): Beneficiaries receiving fees and network control; bear costs of maintaining full nodes and managing block space
 *   - Low-value payment users (powerless/trapped): Primary victims — priced off base layer by limited capacity and rising fees; no viable alternative within Bitcoin itself
 *   - Merchant adoption ecosystem (moderate/constrained): Secondary victims — must choose between payment friction (high fees, slow confirmation) or off-chain intermediaries (counterparty risk, custody)
 *   - Core protocol developers (institutional/constrained): Narrative authority and beneficiary through interpretive control; constrained by network consensus requirements and institutional inertia
 *   - Alternative cryptocurrency projects (moderate/mobile): Demonstrate the constraint is not natural law; their adoption of different trade-offs proves the choice is contingent
 *   - Analytical observer (analytical/analytical): Risks naturalizing a governance choice as an immutable property of distributed consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.48).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Store-of-Value Reading: Decentralization and Full-Node Verifiability as Binding Constraints").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '5ebbaa67-6099-49fd-b53d-800b78574cc0').
narrative_ontology:cs_kernel_codification('5ebbaa67-6099-49fd-b53d-800b78574cc0', fixed_text).
narrative_ontology:cs_authority_grounding('5ebbaa67-6099-49fd-b53d-800b78574cc0', lineage).
narrative_ontology:cs_interpretation_layer_present('5ebbaa67-6099-49fd-b53d-800b78574cc0').
narrative_ontology:cs_reading_relation('5ebbaa67-6099-49fd-b53d-800b78574cc0', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_axiom('5ebbaa67-6099-49fd-b53d-800b78574cc0', foundational, decentralization_binds_capacity).
narrative_ontology:cs_axiom_status(decentralization_binds_capacity, holdable).
narrative_ontology:cs_axiom_grounding('5ebbaa67-6099-49fd-b53d-800b78574cc0', decentralization_binds_capacity, empirically_contingent).
narrative_ontology:cs_axiom('5ebbaa67-6099-49fd-b53d-800b78574cc0', foundational, censorship_resistance_primacy).
narrative_ontology:cs_axiom_status(censorship_resistance_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5ebbaa67-6099-49fd-b53d-800b78574cc0', censorship_resistance_primacy, deontological).
narrative_ontology:cs_reference_frame('5ebbaa67-6099-49fd-b53d-800b78574cc0', decentralized_store_of_value_with_eventual_payment).
narrative_ontology:cs_drift_state('5ebbaa67-6099-49fd-b53d-800b78574cc0', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5ebbaa67-6099-49fd-b53d-800b78574cc0', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, mining_pools).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, payment_use_case).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, merchant_adoption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-VALUE PAYMENT USERS (SNARE) — Users seeking to transact in small amounts or at high frequency are structurally locked out by rising on-chain fees and 1MB block cap. No arbitrage or exit: using Bitcoin becomes economically infeasible; forced to exit Bitcoin entirely or accept off-chain intermediaries (Lightning Network, custodial exchanges). The constraint extracts from this cohort by pricing them off the base layer.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MERCHANT ADOPTION ECOSYSTEM (TANGLED ROPE) — Merchants wanting Bitcoin payment settlement face coordination benefits (censorship resistance, direct settlement without intermediaries) alongside significant extraction (high fees, confirmation delays, complex integration). Constrained by technical complexity and customer demand for low-cost payments. The constraint both enables (coordination via direct settlement) and burdens (off-chain solutions required for practical payments).
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LONG-TERM HOLDERS (ROPE) — Agents focused on wealth preservation and portfolio diversification experience Bitcoin primarily as pure coordination. The constraint (limited on-chain capacity, high verification costs) serves their interests by enforcing scarcity, preventing payment inflation, and maintaining network security through full-node participation incentives. This cohort can arbitrage by holding and benefiting from price appreciation without payment-use friction.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING AND NODE INFRASTRUCTURE (TANGLED ROPE) — Mining pools and full-node operators benefit from the constraint enforcement (fees rise, scarcity maintained, security model protected) but bear costs (infrastructure complexity, regulatory exposure, transaction censorship responsibility). Organized and institutionalized, with strong coordination function (securing the ledger) but also clear extraction benefits (fee capture, block rewards, network control).
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORE PROTOCOL DEVELOPERS (TANGLED ROPE) — The developers and researchers maintaining Bitcoin's protocol (the 'Satoshi vision' interpreters) experience the constraint as both coordination (enforcing the original design principles) and extraction (they hold significant Bitcoin, benefit from scarcity, and wield narrative authority over what 'real Bitcoin' means). Constrained by network consensus requirements but with outsized influence over the interpretation and enforcement of the whitepaper's intent.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE CRYPTOCURRENCY PROJECTS (PITON) — Projects like Litecoin, Monero, or altcoins pursuing higher on-chain throughput or different transaction models experience Bitcoin's constraint as degraded institutional inertia. They see Bitcoin as maintaining the 1MB block cap and decentralization-over-capacity trade-off largely through cultural momentum and brand authority rather than technical necessity. Their existence demonstrates the constraint is not a natural law — alternatives have chosen different trade-offs.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, decentralization and high on-chain capacity may appear to be in inherent tension due to blockchain physics: verifying more data requires more computational resources, which raises barriers to running full nodes. The constraint appears as an immutable property of distributed consensus. However, the structural data reveals this as potential false summit — the specific choice to retain 1MB blocks is a governance decision, not a law of nature. Alternative implementations (higher block sizes, sharding, rollups) demonstrate the trade-off is negotiable.
constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bitcoin_whitepaper_purpose__store_of_value_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, TR),
    TR >= 0.70.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. In the early years (2011), extractiveness was low — the network was not full, fees were negligible, and on-chain capacity was not binding. As adoption grew and the block space became scarce (2017 scaling debate, 2023 ordinals/BRC-20 activation), extractiveness rose. The constraint now extracts by forcing users into tiers: wealthy investors and institutions transact on-chain; low-value users and payment use cases are priced off. The rising trajectory reflects not a change in the rule (1MB blocks remain fixed) but a change in how binding that rule has become as demand exceeded supply. Suppression (0.48): Moderate and rising. Early suppression was low because the constraint was not yet binding. Current suppression includes: technical barriers to running full nodes (hardware requirements, bandwidth), ecosystem fragmentation (not all wallets support Lightning or sidechains), regulatory uncertainty around off-chain solutions, and social pressure against capacity increases (which supporters frame as diluting the decentralization principle). The suppression is not total — users can exit to altcoins or custodial solutions — but the friction is substantial. Theater ratio (0.38): Low-to-moderate. The constraint has relatively high functional content (full nodes do verify the ledger; decentralization does prevent unilateral rule changes) but exhibits some performative elements. Theater has crept upward as the narrative emphasis on 'Satoshi's vision' and 'the original design' increasingly functions as justification independent of whether the current implementation still achieves superior decentralization outcomes. The constraint's legitimacy increasingly rests on historical authority rather than continuous verification of decentralization benefits.
 *
 * PERSPECTIVAL GAP:
 *   The store-of-value reading produces dramatically different classifications depending on who is positioned as the observer. Long-term holders experience pure coordination (Rope) because the constraint serves their goals without friction. Low-value users experience pure extraction (Snare) because they are locked out with no realistic exit. Merchants experience mixed coordination-extraction (Tangled Rope) — they benefit from settlement finality and censorship resistance but suffer from capacity constraints. Mining pools experience mixed benefits and costs (Tangled Rope). The alternative cryptocurrency projects see the constraint as degraded institutional inertia (Piton) — they have chosen different trade-offs, proving it is not immutable. The analytical observer risks seeing a natural law (Mountain) when in fact the 1MB block choice was a governance decision. The widest gap is between the powerless (Snare) and the institutional beneficiaries (Rope) — the same constraint is experienced as pure extraction by one cohort and pure coordination by another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's power level, exit options, and beneficiary/victim status. Long-term holders: institutional power + arbitrage exit + beneficiary status → low d → low experienced extraction (Rope). Low-value users: powerless + trapped exit + victim status → high d → high experienced extraction (Snare, maximum chi). Merchants: moderate power + constrained exit + mixed beneficiary/victim status → moderate d → moderate extraction (Tangled Rope). The core mechanism: beneficiaries of the constraint have arbitrage options (they can access Lightning, exchanges, custodial wallets) and thus experience low or negative effective extraction chi, even though the base extractiveness is substantial. Victims lack arbitrage options — they must either accept the constraint or exit Bitcoin entirely — and experience high chi. The constraint's directionality profile is highly asymmetric: it systematically favors large holders over payment users.
 *
 * MANDATROPHY ANALYSIS:
 *   The store-of-value reading avoids mandatrophy by maintaining genuine coordination benefits (censorship resistance, settlement finality, decentralized consensus) alongside measurable extraction (fee capture from low-value users, price appreciation for early holders). The constraint is not pure extraction masquerading as coordination; nor is it pure coordination inappropriately taxing low-value use cases. The Tangled Rope classification is structurally justified. However, the reading faces a mandatrophy risk: as the gap between payment costs and store-of-value benefits widens, the constraint may reclassify toward Snare if it becomes clear that off-chain solutions cannot actually preserve decentralization properties. The measurement trajectory (extractiveness rising from 0.18 to 0.52 over 12 years) shows the constraint becoming more extractive over time — whether this remains justified depends on the resolution of the off-chain substitutability omega. If Lightning Network fails to preserve decentralization and users are forced into custodial intermediaries, the constraint would reclassify as Snare with mandatrophy unresolved. The reading survives mandatrophy review only if off-chain scaling truly maintains the coordination benefits promised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralization_definition_contention,
    'What constitutes ''sufficient decentralization'' in practice — minimum number of validating nodes, geographic distribution, hardware requirements for participation, or resistance to state coercion?',
    'Empirical measurement of node distribution and validator requirements across periods of network stress (fee spikes, scaling debates); analysis of which metrics correlate with actual censorship resistance outcomes.',
    'If full-node accessibility becomes the binding metric: constraint classification shifts toward Snare (fees lock out low-income participation, reducing practical decentralization). If state coercion resistance is binding: constraint classification may remain Rope (current design achieves sufficient resistance). Definition contention is the core of the reading dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_definition_contention, conceptual, 'The operational definition of ''sufficient decentralization'' is contested between readings').

omega_variable(
    off_chain_layer_substitutability,
    'Do off-chain scaling solutions (Lightning Network, rollups, sidechains) truly preserve decentralization benefits, or do they reintroduce trusted intermediaries and counterparty risk at the scaling layer?',
    'Comparative analysis of Lightning Network routing failures, hub concentration, liquidity requirements, and watchtower trust assumptions; longitudinal tracking of whether off-chain adoption pattern replicates traditional payment intermediation hierarchies.',
    'If off-chain solutions preserve decentralization: the store-of-value reading is vindicated — limited on-chain capacity with off-chain scaling maintains both decentralization and eventual payment capability. If off-chain solutions fail the decentralization test: the constraint reclassifies toward Snare because it imposes decentralization costs without providing decentralization benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(off_chain_layer_substitutability, empirical, 'Whether off-chain scaling truly preserves decentralization properties').

omega_variable(
    sibling_reading_empirical_foreclosure,
    'Does the electronic-cash reading''s bet on higher on-chain capacity (Bitcoin Cash, forks with larger blocks) empirically demonstrate that the store-of-value reading''s constraints are unnecessary, or do those forks show degraded decentralization properties validating the original constraint design?',
    'Node distribution, geographic concentration, hardware requirements for validators, and state-coercion resistance outcomes in larger-block implementations; correlation between adoption and decentralization metrics.',
    'If larger-block forks maintain comparable decentralization: the store-of-value reading''s empirical case weakens — capacity is not intrinsically opposed to decentralization. The sibling reading would coexist rather than be foreclosed. If larger-block forks show degraded decentralization: the store-of-value reading''s axioms are empirically validated — capacity does trade off against decentralization in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_foreclosure, empirical, 'Empirical test whether larger-block designs validate or invalidate the store-of-value reading''s decentralization claims').

omega_variable(
    whitepaper_original_intent_recovery,
    'Was Satoshi Nakamoto''s original design intent (inferred from the 2008 whitepaper and early implementation) fundamentally oriented toward decentralized store of value with eventual payment via off-chain scaling, or was payment the primary use case with decentralization a means to that end?',
    'Textual analysis of Satoshi''s writings; historical reconstruction of design choices (1MB block selection, block time, scripting language limitations); comparison with contemporaneous alternatives to determine path-dependent choices vs. deliberate constraints.',
    'If store-of-value intent is confirmed: the reading''s legitimacy increases — it follows the original design trajectory. If payment intent is confirmed: the reading is a post-hoc reinterpretation, and the electronic-cash reading has stronger grounding. Either way, this is a committer-axis question, not a structural one — the classification outcome does not change, but the axiom status (holdable vs. overridden) may shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_original_intent_recovery, conceptual, 'Satoshi''s original design intent regarding payment vs. store-of-value purpose').

omega_variable(
    moral_hazard_of_settlement_finality,
    'Does the constraint''s enforcement of settlement finality (irreversible transactions after ~10 minutes) create moral hazard by forcing merchants to accept either payment risk or off-chain intermediaries, or does it prevent the larger moral hazard of inflation-prone payment networks?',
    'Analysis of merchant dispute rates and fraud patterns under different settlement models (on-chain vs. off-chain vs. custodial); comparison of hyperinflation regimes using cryptographic scarcity vs. fiat with malleable supply.',
    'If on-chain settlement finality reduces systemic moral hazard: the constraint is justified by coordination benefits. If off-chain intermediaries recreate traditional payment risk: the constraint merely relocates the problem rather than solving it. This affects whether the Tangled Rope classification is sustainable or collapses toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_of_settlement_finality, empirical, 'Whether the constraint''s settlement finality rules reduce or relocate moral hazard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_sov_theater_2011, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(btc_sov_theater_2017, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(btc_sov_theater_2023, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(btc_sov_extract_2011, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(btc_sov_extract_2017, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(btc_sov_extract_2023, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(btc_sov_suppress_2011, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(btc_sov_suppress_2017, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(btc_sov_suppress_2023, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, global_infrastructure).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into two constraint stories with different epsilon values and structural properties. The store-of-value reading (this file) focuses on decentralization as binding and treats capacity as subordinate; epsilon = 0.52, classification Tangled Rope at institutional beneficiary level. The electronic-cash reading (sibling constraint) prioritizes payment functionality and treats decentralization as secondary; epsilon would differ (expected ~0.65, Snare from low-value users' perspective) and victim/beneficiary relationships would invert. The readings are structurally distinct constraints arising from the same kernel, related by the reading_relations edges in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__store_of_value_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
