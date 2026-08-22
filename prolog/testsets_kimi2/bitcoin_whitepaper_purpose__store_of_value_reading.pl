% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__store_of_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bitcoin Store-of-Value Base-Layer Capacity Constraint
 *   domain: distributed_systems/monetary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the store_of_value_reading of the
 *   bitcoin_whitepaper_purpose kernel. The reading treats decentralization
 *   and full-node verifiability as non-negotiable binding constraints,
 *   subordinating on-chain transaction capacity to these goals via the
 *   effective block size limit. The result is a base layer that functions as
 *   scarce settlement infrastructure, pricing low-value users out of on-chain
 *   space and routing them toward off-chain layers such as the Lightning
 *   Network. The constraint is contested: long-term holders and node
 *   operators benefit from the scarcity and low resource requirements, while
 *   users needing cheap on-chain settlement bear the costs of exclusion and
 *   fee-market volatility.
 *
 * KEY AGENTS:
 *   - long_term_holders: Primary beneficiary (powerful/mobile) â store wealth and benefit from the hard-cap, decentralized verification model.
 *   - full_node_operators: Primary beneficiary (organized/constrained) â verify the chain and benefit from the small-block regime that keeps validation costs personal rather than institutional.
 *   - low_value_on_chain_users: Primary target (powerless/constrained) â require cheap censorship-resistant settlement and are priced out by fee spikes.
 *   - core_protocol_maintainers: Agenda setter (organized/mobile) â maintain the reference client and consensus rules that enforce the capacity constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__store_of_value_reading, 0.72).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__store_of_value_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__store_of_value_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__store_of_value_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__store_of_value_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__store_of_value_reading, "Bitcoin Store-of-Value Base-Layer Capacity Constraint").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__store_of_value_reading, "distributed_systems/monetary_theory").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__store_of_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__store_of_value_reading, '782ad6cc-76f9-4995-8d48-e272e96459a3').
narrative_ontology:cs_kernel_codification('782ad6cc-76f9-4995-8d48-e272e96459a3', fixed_text).
narrative_ontology:cs_authority_grounding('782ad6cc-76f9-4995-8d48-e272e96459a3', distributed).
narrative_ontology:cs_reading_relation('782ad6cc-76f9-4995-8d48-e272e96459a3', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('782ad6cc-76f9-4995-8d48-e272e96459a3', bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, coexists_with).
narrative_ontology:cs_axiom('782ad6cc-76f9-4995-8d48-e272e96459a3', foundational, decentralization_requires_capacity_constraints).
narrative_ontology:cs_axiom_status(decentralization_requires_capacity_constraints, holdable).
narrative_ontology:cs_axiom_grounding('782ad6cc-76f9-4995-8d48-e272e96459a3', decentralization_requires_capacity_constraints, empirically_contingent).
narrative_ontology:cs_axiom('782ad6cc-76f9-4995-8d48-e272e96459a3', foundational, settlement_priority_over_retail_payments).
narrative_ontology:cs_axiom_status(settlement_priority_over_retail_payments, holdable).
narrative_ontology:cs_axiom_grounding('782ad6cc-76f9-4995-8d48-e272e96459a3', settlement_priority_over_retail_payments, conventional).
narrative_ontology:cs_reference_frame('782ad6cc-76f9-4995-8d48-e272e96459a3', censorship_resistant_settlement_layer).
narrative_ontology:cs_drift_state('782ad6cc-76f9-4995-8d48-e272e96459a3', contemporary_lightning_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('782ad6cc-76f9-4995-8d48-e272e96459a3', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin as a long-term store of value and censorship-resistant savings vehicle. Benefit from hard-capped supply and from a fee market that sustains security without dilution. Their capital is mobile across markets, but their monetary thesis depends on the base layer remaining decentralized and costly to alter.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Run fully validating nodes to verify every block and transaction without trusting intermediaries. Benefit from the small block weight limit that keeps hardware, bandwidth, and storage requirements within reach of individuals. Their continued participation depends on the constrained capacity; exiting to light-client mode sacrifices sovereignty.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, full_node_operators, beneficiary,
    organized, generational, constrained, global).

% Need to settle small-value transactions on the base layer for remittances, savings, or commerce. Are priced out during fee spikes and forced into custodial Lightning services, off-chain exchanges, or alternative chains. Their need for trustless settlement is constant, but the fee market treats their use case as infra-marginal.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, low_value_on_chain_users, payer,
    powerless, immediate, constrained, global).

% Maintain the Bitcoin Core reference implementation and the BIP process. Enforce the consensus rules that keep block weight limited. Their influence rests on social acceptance by node operators and miners; they can exit to other projects, but their reputational capital is bound to the current consensus.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__store_of_value_reading, core_protocol_maintainers, agenda_setter,
    organized, generational, mobile, global).

narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__store_of_value_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves permissionless, institutionally independent verification of a global monetary ledger by keeping the resource cost of full validation low enough that individuals can participate without data-center infrastructure.
% TRANSFER_FUNCTION: Transfers the burden of network security from block subsidy to a fee market, pricing low-value transactional demand off the base layer and redirecting it to off-chain layers or custodial alternatives; moves settlement priority to agents willing and able to pay prevailing fees.
% ABSENT_VOICES: Small merchants and individuals in high-inflation economies requiring sub-dollar on-chain settlement; early adopters who understood the whitepaper's title as a mandate for everyday electronic cash; rival fork communities advocating larger blocks who were socially and economically marginalized during the block size wars.
% DISAPPEARANCE_RATIONALE: If the constraint vanishedâif block capacity were no longer subordinated to full-node verifiabilityâbase-layer fees would collapse for small transactions, the Lightning Network's competitive advantage would erode, mining economics would shift toward volume rather than fee premium, and the social contract around monetary scarcity and decentralization would reorganize around a different security model.
% FOUNDING_PROBLEM: How to create a peer-to-peer electronic cash system that allows online payments to be sent directly from one party to another without going through a financial institution.
% FOUNDING_PROBLEM_CORROBORATION: The whitepaper title and abstract corroborate the electronic cash framing from the origin. Long-term holders and node operators corroborate the claim that the founding problem is best solved by prioritizing settlement sovereignty over retail payments. Electronic cash advocates and independent historians corroborate the claim that the arrangement has drifted from the original purpose. No single corroborating source is outside all benefiting parties; the dispute is structural.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__store_of_value_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__store_of_value_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__store_of_value_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__store_of_value_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint structurally prices a class of users out of the base layer, converting block space into a scarce fee-good rather than an open payment rail. Suppression is substantial (0.62): the limit is enforced by consensus rules and by social norms that marginalize big-block advocacy and fork attempts. Theater ratio is moderate (0.35): the decentralization narrative is partially performativeâmining pools are concentrated, and Lightning use is often custodialâyet the coordination function is not purely fictive. Accessibility collapse is high (0.75) because, once a user commits to Bitcoin for censorship-resistant settlement, low-fee on-chain alternatives within the system are structurally absent. Resistance is high (0.70) due to the block size wars, persistent fork communities, and ongoing criticism from the electronic-cash faction. The measurement series show extraction rising as the subsidy declines and fee markets mature, with suppression spiking during the peak of the block size conflict and stabilizing at a permanently elevated level.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (long-term holders, node operators) experience the constraint as a necessary Rope-like condition for monetary sovereignty: without it, the system would collapse into centralized intermediation. The payer seat (low-value on-chain users) experiences the same structure as an asymmetric extraction mechanism that denies them the permissionless access they were led to expect. The engine computes this divergence from the same structural data; the authored claim of tangled_rope does not adjudicate the dispute but names it.
 *
 * DIRECTIONALITY LOGIC:
 *   Long-term holders and full-node operators are declared beneficiaries and receive low directionalityâthe constraint subsidizes their preference for a scarce, decentralized settlement layer. Core protocol maintainers sit near the beneficiary end because they enforce and propagate the rule set that sustains their influence. Low-value on-chain users are declared victims and sit near the full-target end: they bear the effective extraction through fees and exclusion, with constrained exit to layers that sacrifice the self-verifying properties the constraint claims to protect. The asymmetry is not in power alone but in the coupling of capacity scarcity to the stated coordination goal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpeer-to-peer electronic cash without trusted third partiesâis contested. The store_of_value reading claims the problem is still live but best solved by prioritizing settlement and sovereignty over retail payments. If the founding problem were dead (pure electronic cash no longer the goal) and the arrangement persisted solely through inertia, the constraint would drift toward piton. If the coordination justification were entirely false (large blocks would not harm decentralization), it would drift toward snare. The current evidence supports a hybrid: the coordination function is genuine but contested, and the extraction is asymmetric and structurally locked in by the same rules. Tangled rope captures this precisely; rope or snare would each misrepresent one half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whitepaper_purpose_ambiguity,
    'Does the Bitcoin whitepaper kernel inherently prioritize electronic cash functionality, or is it underdetermined between cash and settlement readings?',
    'Linguistic and historical analysis of the whitepaper text and Satoshi''s early communications; comparative analysis of which reading better coheres with the kernel''s technical mechanism and the narrative''s own stated goals.',
    'If the kernel text is underdetermined, neither reading can claim sole legitimacy from the kernel alone; classification shifts from lineage-based authority to distributed social construction, weakening the Mountain-like framing some holders apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whitepaper_purpose_ambiguity, conceptual, 'Ambiguity of the whitepaper''s purpose between cash and store of value.').

omega_variable(
    decentralization_risk_empirical,
    'Would increasing base-layer block capacity materially degrade full-node decentralization given contemporary bandwidth, storage, and compute costs?',
    'Empirical measurement of node count, geographic distribution, sync times, and hardware requirements under varying block size scenarios or observed natural experiments from forked chains.',
    'If the empirical claim fails, the coordination justification for the capacity constraint collapses, shifting classification toward snare; if sustained, the tangled rope framing is strengthened because the genuine coordination function is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_risk_empirical, empirical, 'Empirical basis for the decentralization justification of block size limits.').

omega_variable(
    lightning_sovereignty_drift,
    'Does the Lightning Network''s emergent topology preserve the non-custodial, self-verifying properties that justify pricing users off the base layer?',
    'Network analysis of Lightning channel custody rates, liquidity centralization, routing dependence on large hubs, and user-facing wallet custody defaults.',
    'If Lightning is predominantly custodial or hub-dependent, the off-chain scaling justification becomes a cover for exclusion, amplifying extractiveness; if non-custodial routing dominates, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lightning_sovereignty_drift, empirical, 'Custodial drift in Lightning Network scaling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__store_of_value_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper_purpose__store_of_value_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper_purpose__store_of_value_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper_purpose__store_of_value_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__store_of_value_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__store_of_value_reading, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper_purpose kernel decomposes into at least three structurally distinct constraints: store_of_value_reading (this file), electronic_cash_reading, and nakamoto_oracle_opacity. Each carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family; no single story captures the full kernel because the kernel conflates multiple empirical and normative claims under one colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
