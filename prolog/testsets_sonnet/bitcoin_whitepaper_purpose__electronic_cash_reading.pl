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
    constraint_indexing:directionality_override/3,
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
 *   This story instantiates ONE reading of the contested Bitcoin whitepaper
 *   purpose kernel: the electronic cash reading, which treats the title's
 *   'Peer-to-Peer Electronic Cash System' phrasing as a binding design telos
 *   requiring the protocol to prioritize low-fee, high-throughput everyday
 *   transactional use. Under this reading, block capacity should scale to
 *   accommodate transaction volume, and verification cost is treated as a
 *   variable that can rise if it enables the cash-use case. This is
 *   structurally distinct from the sibling store_of_value_reading (a separate
 *   constraint file), which treats decentralization and full-node
 *   verifiability as the binding constraints and subordinates capacity to
 *   them — that reading has a different beneficiary/victim structure and a
 *   different epsilon, and is NOT described further here except by reference.
 *   This story does not average across readings or hedge between them; it
 *   authors the cash-telos reading as a clean, internally coherent
 *   constraint.
 *
 * KEY AGENTS:
 *   - payment_processors: beneficiary (organized/mobile) — commercial gain from expanded capacity
 *   - low_value_transactors: beneficiary (powerless/constrained) — benefits from low fees but has no governance voice
 *   - node_operators: primary payer (moderate/trapped) — bears rising verification cost
 *   - home_validators: primary payer (powerless/trapped) — priced out of independent verification first
 *   - core_protocol_developers: agenda_setter under pressure, also partially excluded when resisting
 *   - mining_pools: beneficiary and secondary agenda_setter — fee revenue incentive aligns with capacity expansion
 *   - protocol_historians: analytical observer of the textual and historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.42).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.38).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__electronic_cash_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__electronic_cash_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__electronic_cash_reading, "Electronic Cash Reading of the Bitcoin Whitepaper's Founding Telos").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__electronic_cash_reading, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__electronic_cash_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__electronic_cash_reading, '0e168981-861d-4540-8b2b-5c5500113dd2').
narrative_ontology:cs_kernel_codification('0e168981-861d-4540-8b2b-5c5500113dd2', fixed_text).
narrative_ontology:cs_authority_grounding('0e168981-861d-4540-8b2b-5c5500113dd2', distributed).
narrative_ontology:cs_reading_relation('0e168981-861d-4540-8b2b-5c5500113dd2', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e168981-861d-4540-8b2b-5c5500113dd2', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, influences).
narrative_ontology:cs_axiom('0e168981-861d-4540-8b2b-5c5500113dd2', foundational, title_text_binds_transactional_design_priority).
narrative_ontology:cs_axiom_status(title_text_binds_transactional_design_priority, holdable).
narrative_ontology:cs_axiom_grounding('0e168981-861d-4540-8b2b-5c5500113dd2', title_text_binds_transactional_design_priority, conventional).
narrative_ontology:cs_axiom('0e168981-861d-4540-8b2b-5c5500113dd2', secondary, low_fee_everyday_use_is_the_measure_of_protocol_success).
narrative_ontology:cs_axiom_status(low_fee_everyday_use_is_the_measure_of_protocol_success, holdable).
narrative_ontology:cs_axiom_grounding('0e168981-861d-4540-8b2b-5c5500113dd2', low_fee_everyday_use_is_the_measure_of_protocol_success, instrumental).
narrative_ontology:cs_reference_frame('0e168981-861d-4540-8b2b-5c5500113dd2', whitepaper_title_transactional_primacy).
narrative_ontology:cs_drift_state('0e168981-861d-4540-8b2b-5c5500113dd2', post_2017_fee_market_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0e168981-861d-4540-8b2b-5c5500113dd2', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__electronic_cash_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__electronic_cash_reading, home_validators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__electronic_cash_reading, cash_telos_binding_on_protocol_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build merchant payment rails on top of on-chain capacity. Larger blocks and lower per-transaction fees directly lower their operating costs and expand the addressable market of small merchants who could not previously afford on-chain settlement. They lobby actively for capacity increases and fund development aligned with the cash-use case.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Individuals wanting to buy coffee, remit small sums, or transact in regions with weak banking infrastructure. They benefit from low fees and fast confirmation but have no organized voice in protocol governance; they experience the constraint's outcome without shaping its terms.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, low_value_transactors, beneficiary,
    powerless, immediate, constrained, global).

% Small and mid-size businesses that would accept bitcoin directly if transaction costs stayed near zero. Their adoption decisions are contingent on the cash-use case remaining protocol-prioritized; they can exit to other payment rails if fees rise.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, merchant_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Run full nodes to independently verify the chain. Larger blocks driven by the cash telos raise storage, bandwidth, and initial-sync costs, pushing verification out of reach for individuals with modest hardware and home internet connections. Their exit is nominally free (stop running a node) but doing so surrenders the independent verification that gives their coin holdings meaning — a structural trap, not a real alternative.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, node_operators, payer,
    moderate, generational, trapped, global).

% Individual hobbyist and privacy-motivated node runners without institutional resources. Rising resource requirements from capacity expansion price them out first, concentrating verification capacity among well-resourced operators and eroding the distributed verification the system was built to guarantee.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, home_validators, payer,
    powerless, generational, trapped, local).

% Maintain reference implementations and steer technical proposals. Under this reading they are pressured by payment-processor and merchant coalitions to raise capacity limits; some resist on verifiability grounds and are marginalized within cash-telos-aligned development communities as obstructionist.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, core_protocol_developers, agenda_setter,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, core_protocol_developers, excluded).

% Larger blocks with more transaction volume increase fee revenue per block over time as the subsidy halves. They have direct economic incentive to support the cash-use expansion and can signal for capacity increases through hash power.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__electronic_cash_reading, mining_pools, agenda_setter).

% Study the whitepaper text, mailing list archives, and early Nakamoto correspondence to adjudicate which telos the founding document actually commits the protocol to. Their readings feed the ongoing legitimacy contest but do not control it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__electronic_cash_reading, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__electronic_cash_reading, payment_processors).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__electronic_cash_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a payment network capable of handling everyday transactional volume at low per-transaction cost, enabling merchant adoption and remittance use cases that require throughput and fee levels incompatible with a constrained block size.
% TRANSFER_FUNCTION: Moves verification cost from transaction users (who pay lower fees under expanded capacity) to node operators (who absorb higher storage, bandwidth, and sync costs to keep validating the larger chain) — a transfer of burden from the transacting class to the verifying class.
% ABSENT_VOICES: Home validators and future users who would want to independently verify the chain on modest hardware are structurally underrepresented in a governance process weighted toward those with immediate transactional or commercial interest in capacity increases; their objection — that verification concentration undermines the trust-minimization the system exists to provide — is raised mainly by protocol developers resisting capacity increases, not by validators themselves.
% DISAPPEARANCE_RATIONALE: If the cash-telos reading disappeared as a governing interpretation, payment processors and merchant adopters argue the system would revert to a settlement-only instrument unsuitable for the use case the whitepaper's title describes, rearranging their business models; store-of-value proponents argue the world would barely change since capacity constraints were never actually loosened at the base layer, with cash-use migrating to second-layer systems instead.
% FOUNDING_PROBLEM: The 2008 whitepaper described a peer-to-peer electronic cash system explicitly intended to allow online payments without a trusted third party, positioned in its opening lines against the friction and reversibility costs of traditional payment intermediaries.
% FOUNDING_PROBLEM_CORROBORATION: Payment processors and early adopters who transacted with bitcoin as cash in 2010-2013 attest the original use case was genuinely transactional. Node-operator communities and independent protocol historians outside the payment-processor beneficiary set attest the founding problem's transactional framing was superseded in practice by settlement-and-store-of-value use once fee markets emerged post-2017, and that the whitepaper title alone cannot resolve which telos binds absent the author's continued participation.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__electronic_cash_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__electronic_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).
:- end_tests(bitcoin_whitepaper_purpose__electronic_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a genuine but partial transfer: transactional convenience for payers is purchased at the cost of rising verification burden shifted onto node operators, who did not choose this trade-off and whose exit (stop validating) undermines the very trust-minimization the coin's value depends on. Suppression (0.38) is moderate rather than extreme because no one is coercively barred from running a node — but the economics of a larger chain functionally exclude less-resourced operators over time, a suppression mechanism operating through cost rather than prohibition. Theater ratio rises from 0.05 to 0.28 as capacity-increase advocacy increasingly frames itself in decentralization-preserving language ('most people don't need to run a node') that functions more as post-hoc justification than as a technical constraint genuinely respected in design choices.
 *
 * PERSPECTIVAL GAP:
 *   From a payment-processor or merchant-adopter seat, the cash-telos reading looks like straightforward, faithful-to-founding-text coordination: the protocol doing what its title promised. From a home-validator seat, the same capacity expansion looks like a slow-motion transfer of verification cost onto the least-resourced participants, eroding the property (permissionless full verification) that made the coordination trustworthy in the first place. The engine computing divergent per-seat types from this single structural dataset is the expected and intended output — it is not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Payment processors, low-value transactors, and merchant adopters sit near the beneficiary end: the cash-telos reading, if enacted, directly reduces their costs and expands their addressable use case. Node operators and home validators sit near the target end: they bear the verification cost increase that capacity expansion under this reading requires, and their exit option (stop validating) is nominal rather than real, since it forfeits the independent-verification guarantee their holdings and trust model depend on — hence exit_options: trapped rather than mobile for both payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (payments without a trusted third party) is contested as live or dead: it remains technically live in the sense that intermediated payment friction still exists, but the specific mechanism this reading proposes (expanded base-layer capacity) is contested as the right solution, with second-layer systems emerging as an alternative that does not require raising base-layer verification costs. Classifying this as tangled_rope rather than snare preserves the fact that a genuine coordination function exists (payment settlement) even as an asymmetric cost is imposed on a specific class (validators) who do not share proportionately in the benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the whitepaper''s title constitute a binding design telos requiring capacity scaling for cash-like use, or is the title''s ''cash'' language a period-appropriate framing that does not bind subsequent protocol design decisions?',
    'There is no living authoritative interpreter: Nakamoto''s 2011 disappearance (see sibling constraint nakamoto_oracle_opacity) removed the only party positioned to adjudicate this authoritatively. Resolution, if any, would come from durable community consensus over multi-decade timescales, or from revealed behavior (which reading the network''s economic majority actually enacts) rather than textual exegesis.',
    'If the cash-telos reading is judged non-binding, the extraction this story attributes to validator-cost transfer loses its coordination justification and the constraint drifts toward snare (extraction without a legitimating founding-text claim); if judged binding, the tangled_rope classification with genuine coordination function is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the whitepaper title binds the protocol to a transactional-use design constraint.').

omega_variable(
    verification_cost_externality_magnitude,
    'How large is the actual verification-cost burden imposed on home validators under realistic 8MB+ block scenarios, and at what capacity level does full-node operation become genuinely inaccessible to hobbyist hardware?',
    'Empirical measurement of storage growth rates, initial-block-download times, and bandwidth costs at various block-size ceilings, cross-referenced against typical consumer hardware and internet infrastructure over the relevant time horizon.',
    'A low actual burden would support classifying this as closer to rope (minimal real cost to node operators); a high burden supports the tangled_rope or even snare reading where validator exclusion is severe and effectively coerced by economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_externality_magnitude, empirical, 'Empirical magnitude of the cost this reading transfers onto node operators.').

omega_variable(
    second_layer_substitution_effect,
    'If low-fee transactional use migrates to second-layer systems (payment channels, sidechains) rather than base-layer capacity increases, does that resolve the founding problem without requiring the base-layer capacity expansion this reading proposes?',
    'Track adoption and transaction volume on layer-two systems relative to base-layer transaction demand over time; if layer-two absorbs the cash-use case, the founding problem may be judged live-but-solved-elsewhere rather than requiring this reading''s proposed remedy.',
    'If second-layer substitution proves adequate, this reading''s claimed necessity weakens substantially, supporting the founding_problem_status of ''dead'' (as a base-layer design requirement) even if ''live'' as a general payments problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_layer_substitution_effect, empirical, 'Whether layer-two systems substitute for the base-layer capacity this reading requires.').


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
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2009, 0.15).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2012, 0.22).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2018, 0.36).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2021, 0.4).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement(bitc_su_t2012, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2012, 0.18).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(bitc_su_t2018, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2018, 0.34).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2021, 0.37).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper_purpose__electronic_cash_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__electronic_cash_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__electronic_cash_reading, 0.15).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__electronic_cash_reading, nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% Part of a three-member kernel family under bitcoin_whitepaper_purpose. This file (electronic_cash_reading) and its sibling store_of_value_reading are two incompatible-in-practice-but-both-live readings of the same founding text, linked via the coordinating nakamoto_oracle_opacity constraint which documents why no authoritative tiebreaker exists. The two readings are NOT merged into one story with an observable parameter — per the epsilon-invariance principle they carry different beneficiary/victim sets and different extraction profiles and are authored as separate files, cross-linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__electronic_cash_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
