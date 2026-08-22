% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__electronic_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: bitcoin_whitepaper_purpose__electronic_cash_reading
 *   human_readable: Electronic Cash Reading of the Bitcoin Whitepaper's Founding Telos
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story authors ONLY the electronic-cash reading of the Bitcoin
 *   whitepaper's founding kernel: the position that the title's word 'cash'
 *   is a binding design telos, obligating the protocol to prioritize low-fee,
 *   high-throughput everyday transactional use, which in practice means
 *   favoring expanded on-chain capacity. This is a distinct constraint from
 *   the sibling store-of-value reading (a separate story, which treats
 *   decentralization and full-node verifiability as the binding constraint
 *   and subordinates capacity to them) and from the nakamoto_oracle_opacity
 *   constraint (which is about the absence of an authoritative interpreter,
 *   not about a substantive reading). Under electronic-cash's own lights, the
 *   standing arrangement under contest is the current protocol's conservative
 *   capacity policy, which this reading regards as a departure from the
 *   founding mandate — ε is authored for that standing arrangement as this
 *   reading evaluates it, not for the large-block alternative this reading
 *   endorses.
 *
 * KEY AGENTS:
 *   - payment_processors: primary beneficiary (organized/mobile) — needs cheap fast on-chain settlement
 *   - low_value_transactors: primary beneficiary (powerless/constrained) — needs low fees to transact at all
 *   - home_node_operators: primary target (powerless/trapped) — bears rising validation cost from capacity growth
 *   - archival_full_node_operators: secondary target (moderate/constrained) — bears permanent accumulating storage cost
 *   - mining_pools_with_industrial_scale: beneficiary and agenda-setter (institutional/mobile) — captures fee revenue, shapes capacity debates
 *   - store_of_value_reading_adherents: excluded rival faction (organized/mobile) — analytical observer of the sibling reading, structurally discounted here
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Electronic Cash Reading of the Bitcoin Whitepaper's Founding Telos").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e493bc9f-32df-4706-8ac4-7d01b01821f9').
narrative_ontology:cs_kernel_codification('e493bc9f-32df-4706-8ac4-7d01b01821f9', fixed_text).
narrative_ontology:cs_authority_grounding('e493bc9f-32df-4706-8ac4-7d01b01821f9', distributed).
narrative_ontology:cs_reading_relation('e493bc9f-32df-4706-8ac4-7d01b01821f9', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('e493bc9f-32df-4706-8ac4-7d01b01821f9', foundational, cash_telos_is_binding_design_constraint).
narrative_ontology:cs_axiom_status(cash_telos_is_binding_design_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e493bc9f-32df-4706-8ac4-7d01b01821f9', cash_telos_is_binding_design_constraint, conventional).
narrative_ontology:cs_axiom('e493bc9f-32df-4706-8ac4-7d01b01821f9', secondary, transactional_utility_takes_priority_over_verification_cost).
narrative_ontology:cs_axiom_status(transactional_utility_takes_priority_over_verification_cost, holdable).
narrative_ontology:cs_axiom_grounding('e493bc9f-32df-4706-8ac4-7d01b01821f9', transactional_utility_takes_priority_over_verification_cost, instrumental).
narrative_ontology:cs_reference_frame('e493bc9f-32df-4706-8ac4-7d01b01821f9', whitepaper_title_and_abstract_as_literal_mandate).
narrative_ontology:cs_drift_state('e493bc9f-32df-4706-8ac4-7d01b01821f9', post_2017_scaling_conflict_settlement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e493bc9f-32df-4706-8ac4-7d01b01821f9', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_with_industrial_scale).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, home_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, archival_full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, developing_world_users_on_thin_bandwidth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, developing_world_users_on_thin_bandwidth).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, peer_to_peer_electronic_cash_thesis).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, on_chain_scaling_sufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build businesses on the assumption that on-chain transactions will remain cheap and fast enough for point-of-sale and remittance use. A large-block, low-fee chain lets them process high transaction volumes directly on-chain without layered settlement systems, and they lobby protocol governance to keep block capacity expanding with demand.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Want to buy coffee, pay remittances, or move small sums without fees eating the transaction. Their use case only works if base-layer fees stay low, which under this reading requires larger blocks; they have no capacity to run infrastructure themselves and depend entirely on the chain staying cheap to use.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Accept bitcoin as payment for goods and services. Their adoption decision is directly gated by whether transaction confirmation is fast and cheap enough to compete with card networks; they benefit from and advocate for capacity increases that keep the system usable as a medium of exchange rather than a settlement-only asset.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Operate at a scale where storage and bandwidth costs of larger blocks are a rounding error against industrial hashing infrastructure. Higher transaction throughput sustains fee revenue as block subsidy halves over time, and their capital base lets them absorb the propagation costs that smaller validators cannot. They have outsized influence over which capacity proposals gain hashrate signaling support.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_with_industrial_scale, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools_with_industrial_scale, agenda_setter).

% Run full validating nodes from residential connections to independently verify the chain without trusting a third party. Larger blocks directly raise the storage, bandwidth, and initial-sync costs of doing this. As capacity grows, the practical requirements for running a node from home rise, and many operators either upgrade at their own expense or drop off the validating set entirely, ceding verification to whoever remains able to afford it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, home_node_operators, payer,
    powerless, generational, trapped, global).

% Maintain complete historical chain data for auditability, research, and dispute resolution. Every capacity increase compounds their storage burden permanently and irreversibly, since chain history is never pruned in their model. They bear a strictly accumulating cost with no corresponding transactional benefit, since they are not the ones sending small payments.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, archival_full_node_operators, payer,
    moderate, civilizational, constrained, global).

% Benefit in principle from cheap on-chain payments as an alternative to weak local banking infrastructure, but where they attempt to run their own validating infrastructure rather than trust a custodial wallet, thin and metered bandwidth makes keeping pace with a growing chain difficult or impossible, pushing them toward trusting third-party services instead of self-verifying.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, developing_world_users_on_thin_bandwidth, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, developing_world_users_on_thin_bandwidth, beneficiary).

% Write and maintain the software that encodes the capacity rules. Under this reading they face sustained pressure from payment-oriented stakeholders to raise block size limits, and their choices about default parameters directly determine how accessible independent verification remains for ordinary users.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_developers_maintaining_reference_client, agenda_setter,
    institutional, generational, constrained, global).

% Hold that decentralization and universal verifiability are the whitepaper's binding constraint and that capacity expansion trades away the property that makes the asset trustworthy in the first place. Within this reading's own framework their concerns are treated as a subordinate design constraint rather than a competing telos, so their objections are heard but structurally discounted in capacity debates.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_reading_adherents, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a payments network: merchants, processors, and everyday users need a shared, low-friction settlement layer where transaction cost and confirmation time make routine purchases and remittances viable, solving the problem of moving small value peer-to-peer without a trusted intermediary.
% TRANSFER_FUNCTION: Moves verification burden and infrastructure cost from the transacting parties (who get cheap, fast payments) onto the population that self-hosts validation (who absorb rising storage and bandwidth requirements as capacity expands to keep fees low).
% ABSENT_VOICES: Node operators priced out of home validation by rising resource requirements are structurally present in principle (anyone can try to run a node) but practically excluded from the capacity-setting conversation, which is dominated by miners, processors, and merchants whose interests are best served by growth. Store-of-value adherents object loudly but are treated as a minority faction rather than co-owners of the founding telos.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed entirely and capacity growth halted or reversed, payment processors and low-fee merchant use cases would need to migrate to layered settlement systems, degrading the on-chain payment experience; meanwhile home and archival node operators would see their validation costs stabilize or fall. Whether that counts as 'the world rearranging' depends on which constituency you ask — payment-oriented users say yes, decentralization-oriented operators say the rearrangement would be a correction, not a loss.
% FOUNDING_PROBLEM: The whitepaper's stated problem was enabling online payments to be sent directly from one party to another without going through a financial institution, framed explicitly around the word 'cash' and the example of a merchant needing to accept payment without trusting an intermediary.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper's own title and abstract are cited by payment-oriented developers and processors as direct textual evidence the cash-use problem is the founding and still-live mandate. Independent monetary historians and early mailing-list correspondents note Satoshi's later posts discussing both payment and store-of-value framings, and academic protocol historians outside both advocacy camps have documented that the community's own emphasis shifted substantially after 2013-2015 scaling debates — a shift the electronic-cash camp treats as capture by store-of-value interests rather than as the founding problem going dormant.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) reflects that within this reading's own framework, capacity growth transfers real, measurable cost onto node operators to sustain a payments use case that primarily benefits transactors and processors who do not bear that cost. Suppression (0.42) is moderate: home operators are not coercively barred from running nodes, but the accumulating resource requirement functions as a rising practical barrier that behaves like soft suppression over time, which the temporal series (0.10 to 0.42) tracks as capacity pressure intensified through the mid-2010s scaling conflicts and then plateaued once informal capacity-growth advocacy stabilized. Accessibility collapse is moderate-low (0.35) because alternative validation strategies (pruned nodes, trusted-but-verify services, layered settlement) remain available, unlike a true mountain where no alternative exists. Resistance is high (0.72) because this reading's proposed capacity path was and remains actively fought by the sibling reading's adherents and by a substantial share of the developer community — this is a live, contested claim, not a settled one.
 *
 * PERSPECTIVAL GAP:
 *   From the payment-processor and low-value-transactor seats, this reading looks like coordination restoring the whitepaper's actual purpose — a rope correcting course. From the home-node-operator seat, the same capacity expansion looks like an extraction mechanism dressed in founding-text legitimacy: real payment coordination exists, but it is being paid for asymmetrically by people who get no transactional benefit and simply want to verify the ledger they already trust. The engine's tangled_rope computation over these two seats is the intended signal, not an error to reconcile away.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors, low-value transactors, and merchant adopters are structural beneficiaries: the arrangement (as this reading would configure it) subsidizes their use case directly, so directionality sits near the beneficiary end. Home and archival node operators are structural targets: the same capacity-growth policy that lowers fees for transactors raises the resource cost of independent verification for operators, with no compensating benefit flowing back to them — directionality sits near the target end, amplified by their trapped/constrained exit options since verification, once dropped, is difficult to resume from cold storage of missing history. Mining pools sit in a genuinely dual position (beneficiary + agenda_setter) since fee revenue and governance influence both flow to the same seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is authored as contested rather than dead specifically to prevent this reading from being mislabeled as pure zombie extraction: the payments-use problem the whitepaper describes has not disappeared (remittance and point-of-sale demand for low-fee peer-to-peer transfer is empirically real and growing in some markets), so treating this reading as mandatrophy-resolved would understate a genuine live coordination function. Equally, treating it as pure coordination would ignore that node-operator costs are real, rising, and asymmetrically borne — hence tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the whitepaper''s title bind the protocol to prioritize everyday transactional cash-use over decentralization/verifiability, or is ''cash'' better read as an illustrative framing device rather than a binding design constraint?',
    'No single mechanism resolves this because the kernel''s sole authoritative interpreter disappeared in 2011 (see the sibling nakamoto_oracle_opacity constraint). The nearest available evidence is Satoshi''s own mailing-list and forum posts, which contain statements supporting both readings at different times, and the historical fact that early practitioners overwhelmingly used the system for payments before it became primarily a store-of-value instrument, which the two camps interpret oppositely.',
    'If the electronic-cash reading is correct, current conservative capacity policy is a departure from the founding mandate and this constraint''s classification as tangled_rope (real payment coordination plus real extraction from node operators) is the accurate structural account. If the store-of-value reading is correct instead, this reading''s capacity-growth program is better classified as extraction dressed in founding-text legitimacy with a much thinner coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the whitepaper title constitutes a binding telos or an illustrative framing, given the absence of an authoritative interpreter.').

omega_variable(
    node_cost_scaling_severity,
    'At what block-size threshold does home-node validation become genuinely infeasible for a typical residential connection and consumer hardware, versus merely inconvenient?',
    'Empirical measurement of full initial-block-download time, storage growth rate, and bandwidth requirements at various proposed block-size limits, benchmarked against median global residential internet infrastructure over the relevant period.',
    'If the infeasibility threshold is far above any capacity level actually proposed by this reading''s advocates, the extraction claim against node operators weakens substantially and the classification would trend toward rope. If the threshold is near or below proposed levels, the tangled_rope classification with meaningful victim cost is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(node_cost_scaling_severity, empirical, 'Whether proposed capacity increases cross a genuine feasibility threshold for home validation or remain within tolerable inconvenience.').

omega_variable(
    beneficiary_capture_of_founding_narrative,
    'Is the electronic-cash reading''s appeal to founding-text authority a good-faith interpretation, or a post-hoc justification adopted because it happens to serve payment-industry beneficiaries'' commercial interests?',
    'Trace the historical sequence: did electronic-cash advocacy predate or follow the entry of well-capitalized payment-processing and merchant-services interests into the ecosystem? Compare rhetoric and stated priorities across that timeline.',
    'If advocacy substantially predates commercial payment-industry involvement, the reading is better supported as an independent interpretive tradition. If commercial interests demonstrably shaped or amplified the reading''s prominence, the tangled_rope''s extractive component is better understood as capture of a founding narrative for commercial benefit, which would not change the classification but would sharpen the mandatrophy analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_founding_narrative, conceptual, 'Whether appeal to the whitepaper''s founding text is independent interpretation or narrative capture by interested commercial parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__electronic_cash_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2009, 0.15).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2021, 0.54).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2012, 0.18).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% This constraint is one of a three-member kernel family under bitcoin_whitepaper_purpose. electronic_cash_reading (this story) and store_of_value_reading are sibling substantive readings with opposed capacity-priority orderings and different beneficiary/victim structures; each authors its own ε under its own lights per the kernel-reading ε rule. nakamoto_oracle_opacity is not a substantive reading but documents the structural fact that no authoritative interpreter exists to adjudicate between the two substantive readings, which is precisely what keeps both alive simultaneously. All three stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
