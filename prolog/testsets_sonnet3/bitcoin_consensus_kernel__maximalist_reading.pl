% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__maximalist_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__maximalist_reading
 *   human_readable: Maximalist Reading: 21-Million-Cap Immutability as Founding Covenant
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The maximalist reading treats the whitepaper's fixed-issuance monetary
 *   schedule not as a technical parameter chosen among alternatives but as a
 *   founding covenant whose violation would constitute betrayal of the
 *   network's core identity. This reading emerged gradually — early Bitcoin
 *   discourse debated block size and other parameters relatively openly — and
 *   hardened into near-religious doctrine as holder wealth concentrated
 *   around the scarcity narrative. The story authors ONE reading of the
 *   contested bitcoin_consensus_kernel: the maximalist position that
 *   base-layer immutability is a moral and structural absolute, not an
 *   engineering tradeoff. Sibling readings (utility_reading: whitepaper as
 *   minimum-viable, iteratively improvable mechanism; pragmatic_synthesis:
 *   base layer immutable, upper layers free to innovate) are separate
 *   constraint stories with their own ε values, not alternative framings
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - early_adopters_and_miners: primary beneficiaries (organized/arbitrage) — wealth appreciation depends on cap credibility
 *   - large_holders: primary beneficiaries and agenda-setters (institutional/arbitrage) — fund and amplify the covenant narrative
 *   - base_layer_node_operators: agenda-setters (organized/constrained) — enforce the doctrine via consensus-rule rejection
 *   - scalability_researchers: targets (moderate/constrained) — proposals for base-layer change routinely rejected as heresy
 *   - small_transaction_users: targets (powerless/trapped) — bear fee volatility from frozen throughput
 *   - layer_innovation_developers: targets (moderate/constrained) — absorb complexity the base layer refuses to
 *   - protocol_historians: analytical observers — note the doctrine postdates the whitepaper text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.68).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.71).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Maximalist Reading: 21-Million-Cap Immutability as Founding Covenant").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '861370e5-45ee-4d09-91af-73683ef0c323').
narrative_ontology:cs_kernel_codification('861370e5-45ee-4d09-91af-73683ef0c323', fixed_text).
narrative_ontology:cs_authority_grounding('861370e5-45ee-4d09-91af-73683ef0c323', practice).
narrative_ontology:cs_interpretation_layer_present('861370e5-45ee-4d09-91af-73683ef0c323').
narrative_ontology:cs_reading_relation('861370e5-45ee-4d09-91af-73683ef0c323', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('861370e5-45ee-4d09-91af-73683ef0c323', bitcoin_consensus_kernel__pragmatic_synthesis, influences).
narrative_ontology:cs_axiom('861370e5-45ee-4d09-91af-73683ef0c323', foundational, monetary_schedule_is_sacrosanct_covenant).
narrative_ontology:cs_axiom_status(monetary_schedule_is_sacrosanct_covenant, holdable).
narrative_ontology:cs_axiom_grounding('861370e5-45ee-4d09-91af-73683ef0c323', monetary_schedule_is_sacrosanct_covenant, deontological).
narrative_ontology:cs_axiom('861370e5-45ee-4d09-91af-73683ef0c323', secondary, any_base_layer_parameter_change_constitutes_betrayal).
narrative_ontology:cs_axiom_status(any_base_layer_parameter_change_constitutes_betrayal, holdable).
narrative_ontology:cs_axiom_grounding('861370e5-45ee-4d09-91af-73683ef0c323', any_base_layer_parameter_change_constitutes_betrayal, conventional).
narrative_ontology:cs_reference_frame('861370e5-45ee-4d09-91af-73683ef0c323', genesis_block_founding_parameters).
narrative_ontology:cs_drift_state('861370e5-45ee-4d09-91af-73683ef0c323', post_scaling_wars_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('861370e5-45ee-4d09-91af-73683ef0c323', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters_and_miners).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, large_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, base_layer_node_operators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer_innovation_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold coin acquired when issuance was cheap and mining was accessible to consumer hardware; their wealth position depends entirely on the fixed 21-million cap and the fixed issuance schedule never being revisited. They can sell into liquid markets at any time and face no lock-in cost from defending the immutability doctrine — defense is pure upside preservation for them.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters_and_miners, beneficiary,
    organized, civilizational, arbitrage, global).

% Concentrated holdings whose real value is a direct function of guaranteed scarcity. They fund and amplify the social-consensus narrative that any base-layer parameter change is a betrayal of the founding covenant, effectively setting the community agenda through capital, media influence, and node-signaling campaigns, while retaining full ability to exit into other assets at will.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, large_holders, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, large_holders, agenda_setter).

% Run full nodes that enforce consensus rules by rejecting any block violating the fixed monetary schedule. They frame this rejection power as sovereign veto over developers and miners, treating the whitepaper's parameters as scripture. Their own switching cost (re-syncing, community standing, sunk technical identity) makes them structurally reluctant to ever validate a soft or hard fork touching issuance.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, base_layer_node_operators, agenda_setter,
    organized, generational, constrained, global).

% Propose base-layer changes (block size, block interval, fee-market restructuring) to improve throughput or reduce fee volatility. Their proposals are routinely rejected as covenant violations regardless of technical merit; their career and reputational capital is tied to a network that structurally refuses the class of change their research produces, forcing them either to abandon base-layer work or migrate to competing chains at reputational cost.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers, payer,
    moderate, biographical, constrained, global).

% Pay volatile, sometimes prohibitive transaction fees during congestion because base-layer throughput is frozen in the name of monetary-policy purity. They have no vote in the informal node-operator/holder consensus and cannot individually change fee-market conditions; their only exit is paying more, waiting, or leaving the network for a lower-fee alternative that lacks the same liquidity and trust.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, small_transaction_users, payer,
    powerless, immediate, trapped, global).

% Build second-layer or sidechain systems to route around base-layer limits. They must treat the base layer's rules as permanently fixed, absorbing all engineering complexity and risk at the upper layer because the lower layer will not move, and any suggestion that the base layer itself could evolve is treated by the maximalist reading as attacking the network's core value proposition.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer_innovation_developers, payer,
    moderate, biographical, constrained, global).

% Alternative chains and protocol communities that would argue the fixed-cap doctrine is one design choice among several, not an inviolable covenant. Their perspective is structurally excluded from the maximalist reading's internal discourse, which treats non-Bitcoin monetary designs as definitionally illegitimate rather than as a competing engineering tradeoff.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, rival_protocol_communities, excluded,
    organized, generational, mobile, global).

% Study the whitepaper's actual text and the historical record of prior parameter debates and forks. They can observe that the whitepaper itself contains no explicit doctrine of eternal immutability — that reading emerged through subsequent community consensus-formation, not textual mandate — without being party to either the beneficiary or payer positions.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, large_holders).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a hard coordination problem: without a credibly fixed, widely-trusted monetary schedule, a decentralized network of mutually distrusting parties has no Schelling point around which to converge, and the currency's store-of-value function collapses into a contested political football subject to whoever currently controls hashpower or developer mindshare.
% TRANSFER_FUNCTION: Moves scarcity-derived value appreciation to early holders and node operators who benefit from supply-cap credibility, while moving the cost of network inflexibility — congestion fees, foreclosed throughput improvements, forced migration to more complex layered architectures — onto users, researchers, and developers who need the base layer itself to change.
% ABSENT_VOICES: Rival protocol communities and heterodox monetary theorists who would argue immutability is a defensible design choice but not a moral covenant are excluded from the maximalist community's internal legitimacy discourse; their arguments are treated as attacks rather than engineering tradeoffs to be weighed.
% DISAPPEARANCE_RATIONALE: If the maximalist reading of immutability vanished overnight, large holders and node operators dispute that anything of value would be lost (they hold the reading itself constitutes the asset's value proposition), while scalability researchers and layer developers would say the base layer could finally evolve to meet real throughput and fee-market needs — the parties fundamentally disagree about whether the reading is load-bearing infrastructure or an artificial ceiling.
% FOUNDING_PROBLEM: The whitepaper was written to solve double-spending without a trusted third party, using proof-of-work and a capped, predictable issuance schedule to prevent any single party from debasing the currency at will.
% FOUNDING_PROBLEM_CORROBORATION: Node operators and large holders attest the immutability covenant is the network's living core function, still solving the debasement problem daily. Protocol historians and several original mailing-list participants (outside the current beneficiary set) attest the whitepaper text itself specifies a technical mechanism, not a moral prohibition on any future parameter revision, and note the 'immutable covenant' framing hardened into doctrine years after launch, coincident with holder wealth concentration.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the interval because the coordination function (a credible, hard-to-debase money) is real but has been overlaid with an increasingly absolutist enforcement culture that forecloses even upper-layer-compatible improvements when they touch base-layer economics. Suppression (0.71) reflects the social and reputational enforcement mechanism — developers proposing changes face coordinated community rejection, accusations of attacking the network, and exclusion from core development influence, which is a real coercive cost even though no central authority issues formal sanctions. Theater ratio (0.42) captures that a substantial share of 'immutability defense' activity is now performative signaling (social media maximalism, node-count theater) rather than technical necessity — the genuine security rationale for conservative base-layer change has been partly supplanted by identity-protection activity.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters, miners, and large holders sit near the full-beneficiary end: their wealth is a direct function of the covenant's persistence, and they have arbitrage-grade exit (liquid markets) that lets them defend the doctrine costlessly. Node operators are agenda-setters whose enforcement power is real but whose own exit is constrained by identity and technical sunk cost — they are structurally locked into the position they enforce. Scalability researchers, transaction users, and layer developers sit near the full-target end: they bear the cost of frozen parameters (fees, foreclosed throughput, absorbed complexity) without commensurate say in whether the parameters can move.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing centralized debasement without a trusted third party — remains genuinely live; this is not a pure zombie mandate. But the specific DOCTRINE that ANY parameter change (not just debasement) violates the covenant is a mandatrophied expansion: the mechanism designed to prevent one failure mode (arbitrary inflation) has been generalized into a prohibition on all base-layer evolution, including changes that would not touch the monetary schedule at all. The classification as tangled_rope rather than pure snare or pure rope reflects this: genuine coordination value coexists with asymmetric extraction that requires active social enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_warrant_for_absolutism,
    'Does the original whitepaper text itself warrant the maximalist reading''s absolute prohibition on base-layer change, or is the absolutism a subsequent community-constructed doctrine layered onto a more modest technical proposal?',
    'Close textual analysis of the whitepaper combined with historical review of early mailing-list and forum discourse (2009-2013) to establish whether immutability-as-covenant was present at founding or emerged later alongside holder wealth concentration.',
    'If the absolutism is a later construction, the maximalist reading''s legitimacy claim (fidelity to founding text) is substantially weakened, supporting reclassification toward snare; if the whitepaper genuinely specifies the doctrine, the coordination-function claim is stronger and the tangled_rope classification is better justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_warrant_for_absolutism, empirical, 'Whether whitepaper text or later community construction grounds the immutability-as-covenant claim.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the maximalist, utility, and pragmatic-synthesis readings diverge structurally — is it the scope of what counts as ''the kernel'' (base layer only, vs. all consensus parameters), or is it purely a disagreement about revisability?',
    'Formal comparison of each reading''s implied kernel boundary: what set of parameters each reading treats as fixed versus revisable, cross-referenced against actual historical fork proposals and their community reception.',
    'If the disagreement is about kernel scope, the readings are not merely evaluative disagreements but define genuinely different constraints with different victim sets — supporting the decomposition into three separate stories rather than one hedged story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between sibling kernel readings.').

omega_variable(
    holder_concentration_causality,
    'Did the maximalist doctrine cause holder wealth concentration (by locking in scarcity value), or did pre-existing holder concentration cause the maximalist doctrine to be adopted and defended (as a self-interested narrative)?',
    'Historical analysis of wealth-distribution timelines against doctrine-hardening timelines; examination of which actors funded and amplified maximalist messaging campaigns and when.',
    'If concentration preceded and drove the doctrine, this substantially strengthens the tangled_rope-toward-snare reading (extraction disguised as principle); if the doctrine preceded concentration, the coordination-function claim is more credible as prior, not post-hoc.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(holder_concentration_causality, empirical, 'Causal direction between holder wealth concentration and doctrinal hardening.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(bitc_tr_t24, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(bitc_tr_t32, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(bitc_be_t24, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(bitc_be_t32, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(bitc_be_t40, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(bitc_su_t24, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(bitc_su_t32, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(bitc_su_t40, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the bitcoin_consensus_kernel (ε-invariance decomposition). The maximalist reading (this story) authors high extractiveness (0.68) against any base-layer change and a beneficiary/victim structure centered on holder wealth vs. scalability/innovation costs. The utility_reading authors substantially lower extractiveness, treating the whitepaper as an iteratively improvable minimum-viable mechanism. The pragmatic_synthesis sits between the two, authoring immutability only for monetary-schedule parameters while treating upper-layer and even some base-layer non-monetary changes as legitimate. All three share the same underlying kernel (the whitepaper's founding text and consensus mechanism) but diverge in reading, producing different ε, different stakeholders, and different classifications — exactly the pattern the ε-invariance principle requires be decomposed rather than hedged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
