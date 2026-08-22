% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__store_of_value_reading
 *   human_readable: Bitcoin Capacity Subordination Regime (Store-of-Value Reading)
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   The standing arrangement under assessment is Bitcoin's capped
 *   block-capacity regime: a consensus rule, enforced by tens of thousands of
 *   independently operated validating nodes, that holds per-block transaction
 *   space small so that verifying the entire ledger remains within reach of
 *   ordinary hardware. This file instantiates ONE reading of the contested
 *   kernel 'what the whitepaper's design is for': the store_of_value_reading,
 *   which holds that decentralization and full-node verifiability are the
 *   binding goals and that on-chain capacity is legitimately subordinated to
 *   them. The sibling reading (electronic_cash_reading) is a separate
 *   constraint story with its own epsilon and is not averaged into this one;
 *   per the epsilon-invariance principle, this story's epsilon refers to the
 *   capped-capacity arrangement as it actually operates, assessed by this
 *   reading's own lights - which is why a reading that ENDORSES the
 *   arrangement still authors substantial cost: the fee floors that protect
 *   verifiability demonstrably price low-value users off the base layer, push
 *   them toward custodial balances, and route everyday payment volume to the
 *   Lightning Network. The 2015-2017 block-size war is the arrangement's
 *   founding trauma: proposals to raise the limit were fought through client
 *   forks, social-channel moderation conflicts, and finally a chain split,
 *   ending with the cap retained and the opposition expelled to a separate
 *   ledger. KEY AGENTS (by structural relationship): see key_agents.
 *
 * KEY AGENTS:
 *   - long_term_holders: primary beneficiary (organized/mobile) - hold the asset whose scarcity premium the capped, auditable ledger underwrites
 *   - home_full_node_operators: beneficiary (moderate/identity_locked) - perform the independent verification the arrangement exists to keep possible
 *   - large_industrial_miners: fee collector with mixed exposure (institutional/arbitrage) - captures bid revenue for scarce block space, bears the throughput ceiling
 *   - bitcoin_core_contributors: agenda setter (institutional/identity_locked) - gatekeep the reference rules the node network enforces
 *   - low_value_onchain_users: primary target (powerless/constrained) - priced toward second layers or custody when blocks fill
 *   - remittance_dependent_households: acute target (powerless/trapped) - fee spikes tax thin cross-border budgets with no governance voice
 *   - custodial_platform_providers: incidental beneficiary (institutional/arbitrage) - absorb the users the fee floor displaces
 *   - lightning_routing_operators: downstream beneficiary (moderate/mobile) - monetize the payment overflow the cap creates
 *   - big_block_proponents: excluded (powerful/arbitrage) - lost the 2015-2017 conflict and its venues, continue on a forked ledger
 *   - monetary_systems_analyst: analytical observer (analytical/analytical) - measures the decentralization-throughput tradeoff without bearing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.5).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Capacity Subordination Regime (Store-of-Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "economic/technological/governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, 'cf15e9ca-27ee-49a8-8eca-3985895db076').
narrative_ontology:cs_kernel_codification('cf15e9ca-27ee-49a8-8eca-3985895db076', fixed_text).
narrative_ontology:cs_authority_grounding('cf15e9ca-27ee-49a8-8eca-3985895db076', distributed).
narrative_ontology:cs_reading_relation('cf15e9ca-27ee-49a8-8eca-3985895db076', bitcoin_whitepaper_purpose__electronic_cash_reading, forecloses).
narrative_ontology:cs_axiom('cf15e9ca-27ee-49a8-8eca-3985895db076', foundational, onchain_capacity_subordinated_to_decentralization).
narrative_ontology:cs_axiom_status(onchain_capacity_subordinated_to_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('cf15e9ca-27ee-49a8-8eca-3985895db076', onchain_capacity_subordinated_to_decentralization, empirically_contingent).
narrative_ontology:cs_axiom('cf15e9ca-27ee-49a8-8eca-3985895db076', foundational, individual_full_node_verifiability_non_negotiable).
narrative_ontology:cs_axiom_status(individual_full_node_verifiability_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('cf15e9ca-27ee-49a8-8eca-3985895db076', individual_full_node_verifiability_non_negotiable, instrumental).
narrative_ontology:cs_reference_frame('cf15e9ca-27ee-49a8-8eca-3985895db076', decentralization_first_scarcity_design).
narrative_ontology:cs_drift_state('cf15e9ca-27ee-49a8-8eca-3985895db076', post_blocksize_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf15e9ca-27ee-49a8-8eca-3985895db076', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, home_full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, large_industrial_miners).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_dependent_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, custodial_platform_providers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_routing_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, large_industrial_miners).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, digital_scarcity_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, unforgeable_costliness_principle).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__store_of_value_reading, self_custody_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as multi-year savings. Every increment of scarcity credibility the capped, independently auditable ledger preserves flows into the asset's monetary premium. They fund advocacy organizations, operate media channels, and coordinate signaling around protocol proposals. Selling is always technically available, but selling forfeits the exact position the arrangement protects, so their participation stays continuous and vocal.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    organized, generational, mobile, global).

% Run validating software on consumer hardware, checking every block against the rules themselves. The practice costs electricity and attention and pays nothing directly; what it returns is independence from having to trust anyone else's copy of the ledger. Stopping is technically trivial, but the practice is fused with a self-conception of not deferring to institutions, so most who adopt it keep it running for years and treat hardware upgrades as obligations.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, home_full_node_operators, beneficiary,
    moderate, biographical, identity_locked, global).

% Operate warehouse-scale hashing fleets and collect the fees users bid for block inclusion alongside the scheduled issuance. Capped block space concentrates bidding among the highest-value transactions, fattening per-block fee revenue during congestion; the same cap also ceilings how many transactions they can ever process. Their capital moves freely across coins and jurisdictions, so they follow revenue wherever it clears.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, large_industrial_miners, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__store_of_value_reading, large_industrial_miners, payer).

% Maintain the reference implementation that most validating nodes run. They review and merge changes touching the consensus rules, and their review standards determine which proposals ever reach the network. Many have spent a decade or more inside the codebase; their professional reputations are built on the design philosophy the current rules embody, and departing would mean abandoning the work that defines their standing in the field.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_core_contributors, agenda_setter,
    institutional, generational, identity_locked, global).

% Want to move small amounts on the base ledger - funding a payment channel, settling a personal transfer, taking self-custody for the first time. When blocks fill they must outbid high-value traffic or wait indefinitely. Most respond by handing keys to a custodial app or switching to another ledger; remaining self-sovereign at small balances is the option the fee floor removes.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_onchain_users, payer,
    powerless, immediate, constrained, global).

% Use the ledger to send cross-border family support where banking corridors are slow, expensive, or hostile. Fee spikes can consume a meaningful slice of a week's wage, and the alternative corridors available to them are frequently worse. They hold no seat in any governance venue and no technical recourse; their circumstances surface mainly when researchers sample remittance corridors.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, remittance_dependent_households, payer,
    powerless, immediate, trapped, regional).

% Operate exchanges and wallet applications that hold keys on behalf of users. Every balance-sized transaction pushed off the base ledger by fee floors lands in an account they control, deepening the deposit base they lend against and the trading flow they monetize. They defend the status quo in practice while marketing self-custody features at the margins of their product lines.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, custodial_platform_providers, beneficiary,
    institutional, biographical, arbitrage, global).

% Lock capital into payment channels and earn routing fees moving payments that no longer fit economically on the base ledger. The on-layer fee environment is what sends them their volume; their business exists in the shadow of the capacity ceiling. They can redeploy locked capital elsewhere if the economics sour, at the cost of closing channels.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, lightning_routing_operators, beneficiary,
    moderate, biographical, mobile, global).

% Argued through 2015-2017 that raising the block limit was the faithful reading of the original design, built alternative clients, and were progressively frozen out of development channels, forums, and conferences before taking the dispute to a chain split. Those who persisted now operate on a separate ledger; within the main framework their position retains no standing venue.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, big_block_proponents, excluded,
    powerful, biographical, arbitrage, global).

% Studies the tradeoff between validator decentralization and transactional throughput across ledgers, publishing fee-market data, node-distribution surveys, and comparative histories of the 2015-2017 conflict. Holds no position dependent on the outcome and can characterize the arrangement's costs and benefits without bearing either.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, monetary_systems_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__store_of_value_reading, large_industrial_miners).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps independent verification of the shared ledger affordable enough that no participant must trust another's honesty: by holding per-block data small, any motivated individual can audit the entire money supply and rule-set on ordinary hardware, which is what makes the ledger's scarcity claims checkable rather than promised.
% TRANSFER_FUNCTION: Moves fee revenue from users bidding for scarce block inclusion to miners; moves small-value payment activity off the base ledger onto second layers and custodial balances; and preserves the credibility of the outstanding supply as a fixed quantity for savers.
% ABSENT_VOICES: Everyday spenders - the audience the whitepaper originally addressed - have no seat in protocol governance; their interests are voiced mainly by researchers and by the forked-away big-block camp, which lost its venue in the main framework after 2017. Remittance corridors and first-time self-custody seekers appear in studies but at no decision table.
% DISAPPEARANCE_RATIONALE: Remove the capacity cap overnight and blocks balloon, the fee market collapses, full-validation hardware requirements climb until auditing concentrates in datacenters, the holders' scarcity narrative weakens, and custodial and second-layer economics reshuffle - the entire settlement topology reorganizes around whichever new tradeoff emerges.
% FOUNDING_PROBLEM: The whitepaper was built to solve peer-to-peer electronic cash: enabling online payments directly between parties without a trusted financial institution, with the double-spending problem solved by a proof-of-work chain.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper's own title and abstract - authored before any faction existed - attest the cash objective, and contemporaneous founder forum posts describe micro-transaction support as a design target; independent journalistic and academic histories of the 2015-2017 block-size conflict attest that the re-prioritization toward savings-use was achieved over sustained objection rather than consensus. No source outside the holder-operator coalition attests that the cash problem was ever formally retired; the parties dispute whether it was solved, delegated, or abandoned.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__store_of_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: when blocks fill, inclusion goes to the highest bidder, so the effective price of base-layer settlement floats above what small-value uses can pay; that displaced activity is a real, recurring cost imposed on identifiable parties, though part of the fee load is the genuine price of the security service (the resource_allocation Boltzmann floor of 0.15 is respected before calling the remainder overhead). Suppression 0.50 is authored as a RAW STRUCTURAL property, unscaled by power or scope: enforcement is consensus-rule rejection of oversized blocks plus the social machinery (forum moderation, conference exclusion, client marginalization) that settled the 2015-2017 challenge; it peaked during the war and has settled into steady-state rule enforcement. Theater ratio 0.24: verification is real and performed daily by the node network, but a shrinking share of economic actors verify personally - the ratio rises slowly as custody concentrates. Accessibility collapse 0.55: within this framework the alternative (raising the cap) is foreclosed post-schism, but external alternatives persist (competing ledgers, the forked chain), so collapse is partial. Resistance 0.55: open war ended in 2017, but fee complaints, inscription-controversy relitigations, and corridor studies keep friction alive. The measurement series run on ONE shared grid (nine points, T0=2009 genesis era through T16~2025, approximately biennial) so every tracked metric is authored at every examined time point; the suppression series deliberately peaks at T8 (war climax) and decays afterward - enforcement hardened into routine rather than continuing to ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different arrangements from identical structural facts. From the holder and node-operator seats, the capped ledger is the thing that makes the asset verifiable and therefore real - costs borne by small users are the tuition of soundness. From the priced-off user seats, the same rule is a gated door: their payment needs did not shrink, the room did. Miners straddle: fee scarcity fattens per-block revenue while the throughput ceiling caps total billable transactions. Core contributors experience the arrangement as the careful preservation of a fragile consensus property; custodial platforms experience it as customer acquisition. The engine computes these divergences from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (long_term_holders, home_full_node_operators, large_industrial_miners) derive low directionality - the arrangement subsidizes their positions: holders collect scarcity premium, node operators get their verification practice kept affordable, miners collect concentrated bid revenue. Declared victims (low_value_onchain_users, remittance_dependent_households) derive high directionality - they bear the fee floor with constrained or trapped exits, and their displacement is precisely what the enforcement machinery maintains. Miners warrant a note: their realized position mixes fee capture with the throughput ceiling, so their true d sits above the pure-beneficiary derivation; no override is authored because the override mechanism keys on the power atom, and an 'institutional' override would simultaneously distort the core-contributor and custodial-provider seats, which sit at genuinely different positions. The imprecision is recorded here rather than laundered through a coarse correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peer-to-peer electronic cash) is CONTESTED, not dead: this reading holds it was re-specified into a savings-asset problem with cash delegated to layers; the sibling reading holds the re-specification was a takeover. Because the founding problem is contested rather than dead, the mismatch consumer sees status=contested x verdict=world_rearranges - no zombie flag fires, correctly, since the arrangement is presently load-bearing: remove the cap overnight and the settlement topology, fee markets, custody patterns, and the asset's scarcity narrative all rearrange. The piton-risk trajectory runs through the verifiability_exercise_rate omega: if personal verification collapses into specialist ritual while the cap persists, theater_ratio climbs, the coordination function atrophies, and the arrangement drifts toward maintenance-by-inertia - with fixing_cost already prohibitive, since any cap revision requires the very global consensus the schism showed cannot be summoned. Mandatrophy resolution here is prospective, not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the store_of_value_reading of the bitcoin_whitepaper_purpose kernel; what structural arrangement would the sibling electronic_cash_reading produce instead, and which element of the whitepaper does each reading take as binding?',
    'Comparative classification of the sibling constraint story alongside textual analysis locating the disagreement precisely: the title''s ''cash'' telos versus the design sections'' verifiability-first architecture.',
    'If the cash telos is taken as binding, the capacity cap loses its legitimating premise and this arrangement reclassifies toward pure access-gating; if verifiability binds, the arrangement stands as coordination carrying real costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a contested kernel; the sibling reading is a separate constraint with its own epsilon.').

omega_variable(
    satoshi_oracle_absence,
    'Does the founder''s 2011 disappearance leave the whitepaper''s ''cash'' title as a binding telos, or as a superseded artifact of an early design phase?',
    'An authenticated cryptographic signature from Nakamoto''s known keys endorsing a reading, or exhaustive archival analysis of contemporaneous statements weighted by proximity to the design work.',
    'An authenticated cash-telos statement would delegitimate the subordination premise from inside the lineage; authenticated confirmation of the verifiability priority would harden this reading decisively against the sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(satoshi_oracle_absence, empirical, 'Oracle opacity: no living authoritative interpreter exists for the kernel text.').

omega_variable(
    verifiability_exercise_rate,
    'What fraction of the system''s economic value is actually verified by the actors holding it, rather than delegated to custodians and infrastructure operators?',
    'Node-distribution surveys weighted by controlled supply; longitudinal on-chain heuristics separating exchange custody from self-custody holdings.',
    'If self-verification is marginal, the coordination justification thins and a larger share of the measured cost rides on a mostly ceremonial safeguard; if substantial, the cost profile is predominantly genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verifiability_exercise_rate, empirical, 'Whether the verifiability property is exercised or has become specialist ritual.').

omega_variable(
    capacity_limit_naturalness,
    'Is the small-block limit an irreducible consequence of consumer-hardware verification bounds, or a policy point chosen well inside the feasible frontier?',
    'Hardware-cost modeling of full validation at materially larger block sizes; retrospective analysis of the 2015-2017 engineering proposals that sought intermediate positions.',
    'If the limit sits far inside the feasible frontier, subordination is a constructed preference and the extraction component strengthens; if near the frontier, much of the cost is the irreducible price of the verifiability property itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_limit_naturalness, empirical, 'Natural-law versus constructed-character ambiguity of the capacity ceiling.').

omega_variable(
    security_budget_transition,
    'Will fee revenue sustain the hashing security budget as scheduled issuance decays toward zero?',
    'Fee-market elasticity studies across successive halving epochs; stress modeling of security expenditure at projected fee and price levels.',
    'Insufficiency would mean the subordination strategy undermines its own security premise - the binding constraint fails by its own logic - forcing either emission-schedule revision or acceptance of weaker settlement assurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_budget_transition, empirical, 'Whether the fee-market substitution the regime depends on actually closes the security budget.').

omega_variable(
    custodial_ratchet_reversibility,
    'Is the migration of small holders into custodial balances a transient response to fee spikes, or a one-way absorption?',
    'Cohort analysis of withdrawal-to-self-custody behavior as fee regimes fluctuate; natural experiments from wallet UX improvements lowering self-custody friction.',
    'Permanent absorption converts the verifiability property into a specialist service, accelerating the practice drift already recorded and shifting the beneficiary structure toward platform operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_ratchet_reversibility, empirical, 'Reversibility of the displacement of small users from the base layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_sov_reading_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(btc_sov_reading_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.06).
narrative_ontology:measurement(btc_sov_reading_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(btc_sov_reading_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(btc_sov_reading_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(btc_sov_reading_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(btc_sov_reading_tr_t12, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(btc_sov_reading_tr_t14, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 14, 0.23).
narrative_ontology:measurement(btc_sov_reading_tr_t16, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 16, 0.24).

% Extraction over time
narrative_ontology:measurement(btc_sov_reading_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(btc_sov_reading_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.08).
narrative_ontology:measurement(btc_sov_reading_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(btc_sov_reading_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(btc_sov_reading_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(btc_sov_reading_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(btc_sov_reading_be_t12, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(btc_sov_reading_be_t14, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 14, 0.6).
narrative_ontology:measurement(btc_sov_reading_be_t16, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 16, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(btc_sov_reading_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(btc_sov_reading_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.15).
narrative_ontology:measurement(btc_sov_reading_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(btc_sov_reading_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(btc_sov_reading_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(btc_sov_reading_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(btc_sov_reading_su_t12, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(btc_sov_reading_su_t14, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 14, 0.51).
narrative_ontology:measurement(btc_sov_reading_su_t16, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 16, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Bitcoin's design purpose' covers two structurally distinct claims with materially different epsilon values. This story (store_of_value_reading) authors epsilon ~0.62 for the capped-capacity arrangement it endorses; the sibling (electronic_cash_reading) authors epsilon for the same arrangement read as a broken cash system - higher, since under that reading the priced-off user base is the constraint's central casualty rather than its accepted tuition. The upstream text (the whitepaper) feeds both readings; this reading exerts downstream structural pressure on the sibling's operating environment (fee conditions on the main ledger are what make the sibling's framework attractive or irrelevant), which is why the affects_constraints edge runs from this story to the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
