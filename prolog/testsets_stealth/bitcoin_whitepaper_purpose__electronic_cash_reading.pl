% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__electronic_cash_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Electronic Cash Telos Binding (Whitepaper Title Reading)
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the Bitcoin founding text: that
 *   the title's word 'cash' binds, obligating the protocol to serve everyday
 *   transactional use at low fees — which entails expanded on-chain capacity
 *   (multi-megabyte blocks), merchant-payment priority, and acceptance of
 *   rising validator costs. The reading became operative history in the
 *   2015–2018 capacity conflicts: repeated expansion attempts on the main
 *   chain, the August 2017 split that instantiated the program on a minority
 *   chain, and the November 2018 internal war over its further direction. The
 *   colloquial label 'what the whitepaper mandates' decomposes under the
 *   ε-invariance principle into structurally distinct claims; this file
 *   authors ONLY the electronic-cash reading, with the store-of-value and
 *   interpretive-opacity readings as sibling stories linked in
 *   network.affects_constraints. Claim and metrics are independent authored
 *   facts: claimed_type records my structural judgment; the metrics describe
 *   actual operation. KEY AGENTS (by structural relationship): -
 *   scaled_mining_operations: primary beneficiary (institutional/arbitrage) —
 *   collects block rewards and fees; hash mobility lets it capture gains
 *   while shifting exposure - low_value_transactors: principal intended
 *   beneficiary (moderate/mobile) — receives fee relief without bearing
 *   validation costs - payment_processors: secondary beneficiary
 *   (organized/mobile) — monetizes the low-fee rail -
 *   protocol_development_teams: agenda setter (institutional/identity_locked)
 *   — administers the capacity-priority program and could revise it -
 *   node_operators: primary target (moderate/constrained) — bears
 *   uncompensated storage and bandwidth growth - hobbyist_node_operators:
 *   secondary target (powerless/identity_locked) — bears the same costs with
 *   the least resources; coalition leverage historically expressed as exit -
 *   core_developers_small_block_camp: excluded rival (institutional/mobile) —
 *   holds the sibling reading, barred from this arrangement's venues -
 *   monetary_economists: analytical observer (analytical/analytical) — sees
 *   the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.52).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Electronic Cash Telos Binding (Whitepaper Title Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, 'd4371274-335e-43fd-9e09-47850d6cc9f9').
narrative_ontology:cs_kernel_codification('d4371274-335e-43fd-9e09-47850d6cc9f9', fixed_text).
narrative_ontology:cs_authority_grounding('d4371274-335e-43fd-9e09-47850d6cc9f9', lineage).
narrative_ontology:cs_interpretation_layer_present('d4371274-335e-43fd-9e09-47850d6cc9f9').
narrative_ontology:cs_reading_relation('d4371274-335e-43fd-9e09-47850d6cc9f9', bitcoin_whitepaper_purpose__store_of_value_reading, forecloses).
narrative_ontology:cs_reading_relation('d4371274-335e-43fd-9e09-47850d6cc9f9', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('d4371274-335e-43fd-9e09-47850d6cc9f9', foundational, cash_telos_binding).
narrative_ontology:cs_axiom_status(cash_telos_binding, holdable).
narrative_ontology:cs_axiom_grounding('d4371274-335e-43fd-9e09-47850d6cc9f9', cash_telos_binding, conventional).
narrative_ontology:cs_axiom('d4371274-335e-43fd-9e09-47850d6cc9f9', foundational, low_fee_everyday_use_priority).
narrative_ontology:cs_axiom_status(low_fee_everyday_use_priority, holdable).
narrative_ontology:cs_axiom_grounding('d4371274-335e-43fd-9e09-47850d6cc9f9', low_fee_everyday_use_priority, instrumental).
narrative_ontology:cs_reference_frame('d4371274-335e-43fd-9e09-47850d6cc9f9', whitepaper_cash_mandate).
narrative_ontology:cs_drift_state('d4371274-335e-43fd-9e09-47850d6cc9f9', post_hash_war_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d4371274-335e-43fd-9e09-47850d6cc9f9', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, scaled_mining_operations).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_development_teams).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, hobbyist_node_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the big-block client software, schedule recurring hard forks, and publish specifications that invoke the founding text. Corporate patrons and periodic block-reward levies fund them; their professional standing rests on the cash mission remaining the project's governing purpose. Walking away would dissolve the thing they built their careers around.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_development_teams, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_development_teams, beneficiary).

% Operate industrial hashing facilities with capital-intensive hardware and high-bandwidth connections. Larger blocks raise per-block transaction volume and reward superior connectivity; hash fungibility lets them shift machines across same-algorithm chains whenever profitability moves. They financed much of the capacity-expansion advocacy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, scaled_mining_operations, beneficiary,
    institutional, biographical, arbitrage, global).

% Run merchant-facing payment services on the low-fee rail; their margins depend on per-transaction costs staying minimal and confirmations staying predictable. They organize industry working groups, fund adoption campaigns, and press for larger blocks. Leaving means integrating different chains or fiat rails — ordinary commercial re-planning.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Send small remittances and retail payments; near-zero fees are what make their use cases viable, and they rely on wallet providers rather than running their own verification. Trying a different coin or payment app costs nothing but attention.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    moderate, immediate, mobile, global).

% Exchanges, businesses, and service providers running always-on validating nodes. Every capacity increase enlarges the ledger they must store, index, and serve, raising their operating bills with no matching income; abandoning validation means trusting someone else's numbers for solvency-critical checks, and leaving the chain altogether forfeits their place in its economy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    moderate, biographical, constrained, global).

% Keep home machines verifying the ledger as a form of participation. Growing storage and bandwidth demands push them toward pruning modes or shutdown; each step back is experienced as losing the practice they joined to uphold, not saving money they valued.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, hobbyist_node_operators, payer,
    powerless, biographical, identity_locked, global).

% Maintain the rival client line and argued throughout that capacity must yield to verifiability. During the conflicts they were banned from major big-block forums and disinvited from conferences; they shaped events from outside this arrangement's venues and now steward the competing chain.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, core_developers_small_block_camp, excluded,
    institutional, generational, mobile, global).

% Study payment systems and monetary networks at universities and independent institutes. They publish cost, adoption, and governance analyses without taking either camp's funding, and can observe the whole structure including the parts participants dispute.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, scaled_mining_operations).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates trustless value transfer between arbitrary parties without financial intermediaries: distributed timestamping prevents double-spending, and the cash telos directs protocol capacity toward settling high volumes of small transactions at minimal per-transaction cost.
% TRANSFER_FUNCTION: Moves transaction fees and block subsidies from transactors to miners; moves the costs of verification — ledger storage, bandwidth, reindexing — onto full-node operators without compensation; moves payment-processing margins to intermediary services built atop the low-fee rail.
% ABSENT_VOICES: Node operators who exited after the 2017 and 2018 splits no longer sit in the governance conversation; small-block developers were banned or withdrew from big-block venues during the conflicts; future users who inherit a chain whose verification costs price out home validators have no seat at all.
% DISAPPEARANCE_RATIONALE: If the cash-binding commitment vanished overnight, the minority chain loses its organizing justification: hash power reallocates to same-algorithm rivals by profitability, merchant integrations lapse, and the holder base re-prices around whichever purpose the surviving protocol serves. The ecosystem's very partition is evidence that arrangements depend on this reading.
% FOUNDING_PROBLEM: Internet commerce lacked a native trustless payment rail: the founding text proposed peer-to-peer electronic cash that would eliminate trusted-third-party fees and chargebacks for everyday online payment.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper text and the archived early forum record — public and non-partisan — attest the founding problem's content. Attestation that it REMAINS BINDING comes almost entirely from inside the capacity-expansion camp; payment-industry analyses and academic cryptocurrency research outside the benefiting parties document low-fee retail use as one live demand among several, not the undisputed governing purpose. Partial external corroboration of liveness exists; none exists for bindingness.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the capacity-priority program delivers real payment utility while pushing growing storage, bandwidth, and reindex costs onto validators who receive no compensation — real but bounded, far below commission-style rents. Suppression 0.55 is a raw structural property, unscaled by power or scope (only extractiveness is scaled in the engine's computation): the program's persistence requires continuous consensus defense — scheduled hard forks, narrative enforcement of the founding-text reading — which never returned to pre-conflict levels after the 2018 peak. Theater_ratio 0.41: anniversary events, vision rhetoric, and loyalty signaling now outweigh thinning on-chain commerce, though the payment function itself remains real. Accessibility_collapse 0.35: alternatives stayed abundant throughout (the rival chain, layered payment channels, fiat rails, lightweight wallets), so understanding this program never foreclosed exit. Resistance 0.75: the block-size conflicts were among the most contentious episodes in the domain's history. All temporal series run on ONE shared grid (t=0..9: t=0 is 2015 when blocks first approach the ceiling, t=2 the August 2017 split, t=3 the November 2018 hash war, t=9 the 2024 steady state), with every tracked metric authored at every point; the suppression series intentionally traces enforcement intensification and partial decay because enforcement-capacity change is a traced dynamic here. Coalition note: the least-resourced targets were unorganized individually; their collective lever was exercised as exit (remaining on the rival chain in 2017), not as voice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the program as faithful execution of the founding design: from the development teams' position, bigger blocks ARE the whitepaper kept. The payer seats compute the opposite: business node operators face uncompensated cost growth that scales with adoption they do not control, and hobbyist validators — the least resourced targets — historically exercised their only coalition lever, exit, by staying on the rival chain. Beneficiary seats with arbitrage-grade mobility (industrial hashing) sit nearest the subsidized pole: they capture the fee and subsidy flow and can redeploy hardware if the program fails. Same domain, radically different experienced structures — the divergence is computed from power, exit, and declared position, not asserted by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (scaled_mining_operations, payment_processors, low_value_transactors, protocol_development_teams) drive those seats toward the beneficiary pole; industrial hashing's arbitrage exit places it nearest d≈0. Victim declarations (node_operators, hobbyist_node_operators) drive high d; identity-locked hobbyists sit nearer the full-target end than constrained business operators. Global spatial scope modestly amplifies effective extraction for target seats — verification costs compound worldwide while fee relief is competed away. No directionality overrides were needed: the derivation from declared position plus exit options reproduces the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — trustless low-cost payment settlement — is genuinely contested rather than dead: on-chain commerce persists, so resolving mandatrophy as completed would misread a functioning payment rail as vestigial; yet the genealogy interview records that binding-status attestation comes almost entirely from inside the benefiting camp. The tangled_rope classification preserves both truths: the coordination function is real (fee relief, settlement utility), and the same structure transfers uncompensated costs to validators, so neither pure-coordination nor pure-extraction labels fit. The status=contested × verdict=world_rearranges pairing correctly avoids the zombie flag — the arrangement still organizes live activity — while the demand_vs_inertia omega tracks whether that activity is decaying toward theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel bitcoin_whitepaper_purpose (reading: electronic_cash_reading). What changes structurally if the store_of_value_reading prevails instead?',
    'Observe which telos governs concrete protocol decisions — capacity increases versus verifiability-preserving refusals — in each chain''s subsequent upgrade history.',
    'Under the sibling reading the beneficiary and victim sets invert: node operators become the protected class, transactor fee relief becomes subordinate, and this story''s cost asymmetry reverses sign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; the sibling reading would invert the structural delta.').

omega_variable(
    title_telos_binding_force,
    'Does the founding text''s title word ''cash'' carry binding design force, or is it a descriptive label detachable from later protocol obligations?',
    'Hermeneutic weighting of the abstract (''peer-to-peer version of electronic cash''), the body text, and contemporaneous author statements against the text''s silence on block-size policy.',
    'If merely descriptive, this reading''s foundational axiom loses its warrant and the arrangement reduces to a policy preference; if binding, shortfalls in cash performance are fidelity failures rather than design choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(title_telos_binding_force, conceptual, 'Whether the title telos binds or merely describes.').

omega_variable(
    node_cost_materiality,
    'Are the storage and bandwidth burdens that multi-megabyte blocks impose on full-node operators material enough to count as extraction, or negligible coordination overhead?',
    'Cost surveys of full-node operation at multi-MB block sizes versus the 1MB baseline; hardware-threshold analysis of hobbyist participation decline.',
    'If immaterial, the arrangement approaches pure coordination without asymmetric extraction; if material and compounding, the classification drifts snare-ward over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_cost_materiality, empirical, 'Materiality of the validator cost burden relative to delivered utility.').

omega_variable(
    demand_vs_inertia,
    'Is the arrangement sustained by live everyday-payment demand or by committed-holder inertia and sunk institutional investment?',
    'On-chain commerce metrics versus holding-pattern analysis; merchant adoption and attrition series after 2019.',
    'If inertia dominates, theater_ratio rises and the constraint drifts piton-ward as the cash function atrophies behind maintained rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_vs_inertia, empirical, 'Live demand versus institutional inertia sustaining the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btcwp_ecash_tr_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(btcwp_ecash_tr_t1, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 1, 0.24).
narrative_ontology:measurement(btcwp_ecash_tr_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2, 0.33).
narrative_ontology:measurement(btcwp_ecash_tr_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 3, 0.46).
narrative_ontology:measurement(btcwp_ecash_tr_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(btcwp_ecash_tr_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(btcwp_ecash_tr_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 7, 0.37).
narrative_ontology:measurement(btcwp_ecash_tr_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 9, 0.41).

% Extraction over time
narrative_ontology:measurement(btcwp_ecash_be_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(btcwp_ecash_be_t1, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(btcwp_ecash_be_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(btcwp_ecash_be_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(btcwp_ecash_be_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(btcwp_ecash_be_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(btcwp_ecash_be_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(btcwp_ecash_be_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 9, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(btcwp_ecash_su_t0, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(btcwp_ecash_su_t1, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 1, 0.45).
narrative_ontology:measurement(btcwp_ecash_su_t2, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(btcwp_ecash_su_t3, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(btcwp_ecash_su_t4, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(btcwp_ecash_su_t6, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(btcwp_ecash_su_t7, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 7, 0.57).
narrative_ontology:measurement(btcwp_ecash_su_t9, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 9, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the ε-invariance principle: the colloquial label 'what the Bitcoin whitepaper mandates' covers multiple structurally distinct claims with different ε values, beneficiary/victim structures, and failure modes. This story is the electronic-cash reading (capacity-priority; beneficiaries are payment processors, low-value transactors, and scaled hashing; targets are node operators). The store-of-value reading is a separate constraint with inverted structural delta; the oracle-opacity reading addresses the interpretive-authority layer. All three are linked via network.affects_constraints because the upstream textual record (whitepaper plus early forum archive) is cited as evidence by each downstream reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
