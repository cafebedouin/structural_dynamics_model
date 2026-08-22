% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: 21-Million-Cap Monetary Immutability Covenant (Maximalist Reading)
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   This story authors the MAXIMALIST reading of the Bitcoin consensus
 *   kernel: the position that the whitepaper's issuance schedule and monetary
 *   parameters are a founding covenant, immutable in the sense that any
 *   technical proposal to alter them (fee-market redesign, tail emission, cap
 *   adjustment) is treated as an existential attack on the protocol's core
 *   value proposition rather than as an ordinary engineering question. This
 *   is NOT a story about the whole of 'Bitcoin governance' — it is one
 *   specific reading, held by a specific coalition, of a kernel that other
 *   parties read differently (see kernel_context). The ε authored here (0.66)
 *   is for the standing arrangement AS THIS READING SEES IT: a hardened,
 *   socially-enforced immutability norm that increasingly transfers value to
 *   early positions and forecloses adaptation, not for any alternative
 *   arrangement this reading would prefer instead.
 *
 * KEY AGENTS:
 *   - early_holders: Primary beneficiary (organized/arbitrage) — scarcity-derived wealth depends on the covenant holding
 *   - miners_with_sunk_asic_capital: Beneficiary and de facto enforcer (organized/constrained) — sunk capital tied to fee-market trajectory implied by fixed issuance
 *   - core_dev_conservative_faction: Agenda-setter (institutional/identity_locked) — administers the technical veto and is identity-fused with defending immutability
 *   - scalability_layer_developers: Primary target (moderate/constrained) — bears engineering cost of working around a foreclosed base layer
 *   - unbanked_transactional_users: Primary target (powerless/trapped) — promised cash-like utility, receives store-of-value prioritization instead
 *   - protocol_researchers_proposing_monetary_changes: Excluded voice (moderate/trapped) — technically serious proposals treated as heresy
 *   - monetary_policy_researchers: Analytical observer — assesses credible-commitment function vs. constructed dogma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.66).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.71).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "21-Million-Cap Monetary Immutability Covenant (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '1dc90b8d-d385-4a4e-81aa-e2f588e5019e').
narrative_ontology:cs_kernel_codification('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', fixed_text).
narrative_ontology:cs_authority_grounding('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', practice).
narrative_ontology:cs_interpretation_layer_present('1dc90b8d-d385-4a4e-81aa-e2f588e5019e').
narrative_ontology:cs_reading_relation('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', foundational, monetary_parameters_are_unappealable).
narrative_ontology:cs_axiom_status(monetary_parameters_are_unappealable, holdable).
narrative_ontology:cs_axiom_grounding('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', monetary_parameters_are_unappealable, conventional).
narrative_ontology:cs_axiom('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', foundational, any_parameter_change_constitutes_covenant_breach).
narrative_ontology:cs_axiom_status(any_parameter_change_constitutes_covenant_breach, holdable).
narrative_ontology:cs_axiom_grounding('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', any_parameter_change_constitutes_covenant_breach, deontological).
narrative_ontology:cs_reference_frame('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', genesis_block_issuance_schedule).
narrative_ontology:cs_drift_state('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', post_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dc90b8d-d385-4a4e-81aa-e2f588e5019e', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, miners_with_sunk_asic_capital).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, core_dev_conservative_faction).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_layer_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, unbanked_transactional_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_researchers_proposing_monetary_changes).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, sound_money_doctrine).
narrative_ontology:constraint_vindicates(bitcoin_consensus_kernel__maximalist_reading, credible_commitment_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired large positions when the asset was cheap and the 21-million cap was a marketing claim rather than a tested commitment. Their wealth is directly a function of scarcity holding; any credible move to alter issuance or cap parameters threatens the basis of their position. They fund advocacy, media, and conference infrastructure that frames the cap as sacred and any technical discussion of changing it as heresy.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_holders, beneficiary,
    organized, generational, arbitrage, global).

% Have sunk enormous capital into specialized hardware whose value depends on the fee market maturing exactly as the whitepaper's issuance schedule predicts. They enforce the social consensus around the covenant by threatening to fork away from any client that changes core monetary parameters, exercising de facto veto power over protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, miners_with_sunk_asic_capital, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, miners_with_sunk_asic_capital, agenda_setter).

% Maintains the reference client and gatekeeps which changes are considered legitimate. Their professional and reputational identity is fused with having defended the immutability narrative for over a decade; proposing a monetary change would be read by their own community as betrayal. They administer the technical review process that any change must pass, and they are the ones who could in principle propose changes but treat doing so as unthinkable.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, core_dev_conservative_faction, agenda_setter,
    institutional, generational, identity_locked, global).

% Build layer-2 and sidechain workarounds because base-layer throughput and fee-market changes are foreclosed by the covenant. They bear the engineering cost of routing around a constraint they did not choose, and any proposal to relax base-layer parameters to reduce that burden is met with accusations of attacking sound money. Their exit is limited to building elsewhere, abandoning years of accumulated expertise and community standing.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_layer_developers, payer,
    moderate, biographical, constrained, global).

% Were promised peer-to-peer electronic cash for everyday transactions in the original whitepaper's framing, but face high fees and slow confirmation as the network's transactional-use case has been deprioritized relative to its store-of-value use case. They have no voice in protocol governance and no realistic technical alternative that carries the same network effects.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, unbanked_transactional_users, payer,
    powerless, immediate, trapped, global).

% Have proposed technically serious changes (tail emission for long-term security budget, adjusted issuance curves) grounded in game-theoretic concerns about post-subsidy security. They are excluded from serious consideration not on technical merit but because the maximalist reading treats any such proposal as violating covenant, ending careers and inviting harassment rather than debate.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_researchers_proposing_monetary_changes, excluded,
    moderate, biographical, trapped, global).

% Study whether a fixed, unappealable monetary rule is a genuine Schelling point solving a real credible-commitment problem, or a constructed dogma that transfers wealth to early positions while foreclosing adaptation. They observe the social enforcement mechanisms without being subject to them directly.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, monetary_policy_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, early_holders).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine credible-commitment problem: without a hard, unappealable cap, any monetary authority (even a decentralized one) faces pressure to inflate, and market participants cannot price the asset with confidence if the supply schedule is negotiable. Fixing the parameters ex ante and treating them as beyond discussion removes that discretion entirely.
% TRANSFER_FUNCTION: Moves scarcity-derived appreciation from anyone who could have benefited from adaptive monetary or protocol changes (transactional users needing lower fees, researchers wanting a tail-emission security budget, new entrants who would benefit from a more flexible base layer) to those already holding large positions and those whose capital is sunk in the current fee-market structure.
% ABSENT_VOICES: Protocol researchers proposing monetary changes and transactional users in the unbanked/underbanked segment would object that the covenant has calcified from a credible-commitment tool into a wealth-preservation dogma, but they hold no formal governance role and voicing the objection publicly costs them standing in the community whose infrastructure they depend on.
% DISAPPEARANCE_RATIONALE: Believers hold that if the immutability covenant were abandoned, the entire value proposition collapses overnight — the asset becomes just another flexible-supply currency with no differentiated claim on being sound money, and early holders' wealth evaporates. Critics hold that the network's actual utility (settlement, censorship-resistance, transactional use) would persist or even improve under a technically negotiated change, and that only the speculative premium tied to absolute fixedness would be affected. The two camps do not agree on what 'the world' even is here.
% FOUNDING_PROBLEM: The whitepaper needed to solve double-spending without a trusted third party, and needed a credible answer to 'why won't this just be inflated away like every prior digital cash scheme or fiat currency,' to bootstrap trust in a novel, trustless system with no central bank track record to point to.
% FOUNDING_PROBLEM_CORROBORATION: Early holders and the conservative dev faction (the benefiting parties) attest the problem remains live and that any deviation reopens the inflation-trust problem from scratch. Independent monetary economists and protocol researchers outside the holder base — including original mailing-list participants who raised the tail-emission security-budget question years before it became taboo — attest that the founding trust problem was substantially solved by the network's track record and hash-rate security, and that the remaining absolute-immutability stance now serves wealth preservation more than trust-bootstrapping.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.66, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.28 to 0.66 over the interval, tracking the network's transition from an experimental payment system (where immutability was a genuine, contested design bet) to an asset class whose narrative depends on absolute scarcity (where immutability defends realized wealth). Suppression tracks alongside (0.35 to 0.71) as social enforcement mechanisms — reputational attacks on researchers, community ostracism, narrative gatekeeping by media and conference organizers funded by holder interests — hardened into a mature apparatus. Theater ratio rises more modestly (0.15 to 0.42): a genuine credible-commitment function persists (this reading is not claimed as pure snare), but an increasing share of 'defending sound money' activity is performative signaling rather than technical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and sunk-capital miners sit near the full-beneficiary end: the covenant's persistence is the mechanism by which their positions retain and grow value, and their exit options (arbitrage for holders, constrained-but-organized for miners) let them extract without bearing proportional cost. Scalability developers and transactional users sit near the full-target end: they bear engineering and utility costs directly traceable to the same immutability norm, with constrained-to-trapped exit. The conservative dev faction is agenda-setter rather than simple beneficiary — they administer the gate but their capture is identity-based (professional/reputational fusion with the immutability narrative) rather than straightforwardly financial, which is why exit_options is coded identity_locked rather than arbitrage.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not pure snare) preserves the fact that a real coordination problem — credible commitment against monetary discretion — was genuinely solved by the original design and genuinely benefits participants who value predictability. The tangled element is that the SAME structure that solved the original bootstrapping problem now also serves as a wealth-preservation mechanism for early positions and forecloses adaptations (tail emission for long-term security budget, base-layer throughput changes) that serious researchers argue the network may eventually need. Treating this as pure mountain (immutable natural law) would erase the identifiable beneficiaries; treating it as pure snare would erase the genuine coordination function the original whitepaper solved. The maximalist reading, by making the covenant literally unappealable, is the reading most likely to convert a time-bound credible-commitment device into a permanent extraction structure — which is exactly the structural delta this reading is authored to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credible_commitment_vs_wealth_preservation,
    'Is the maximalist immutability stance still functioning as a credible-commitment device solving a live monetary-trust problem, or has it become primarily a wealth-preservation mechanism for early positions now that the network''s trust track record is independently established?',
    'Compare the security-budget game theory (would the network''s hash-rate security genuinely collapse post-subsidy without either fee-market growth or tail emission) against the actual voting/signaling patterns of the coalition defending immutability (does opposition correlate with position size and duration rather than with mainnet security metrics).',
    'If the credible-commitment function is still load-bearing, the tangled_rope classification''s coordination leg is well-supported. If the function is now vestigial and defense correlates with holding size, the constraint is closer to a snare wearing a coordination-function costume, and the maximalist reading''s claimed_type should be revisited downward toward snare in a future story version.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credible_commitment_vs_wealth_preservation, empirical, 'Whether immutability defense still tracks a genuine security-budget problem or now tracks holder wealth.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the maximalist reading diverge from the pragmatic_synthesis and utility readings — is it a disagreement about WHAT the whitepaper committed to (textual/interpretive), or a disagreement about WHETHER any commitment, once made, can ever legitimately be revised (procedural/normative)?',
    'Textual analysis of the original whitepaper''s own hedging language (Nakamoto''s own later mailing-list posts entertained parameter changes) cross-referenced against community constitutional practice (has any monetary parameter ever actually been changed, and under what consensus threshold).',
    'If the disagreement is textual, this reading is vulnerable to originalist counter-argument from within its own tradition (the founding text itself may not support absolute immutability). If procedural, the disagreement is normative all the way down and no textual resolution is possible — the readings simply coexist as competing values commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates whether the maximalist/utility/pragmatic split is interpretive or normative.').

omega_variable(
    identity_lock_reversibility,
    'How reversible is the conservative dev faction''s identity-lock — could a generational turnover in core maintainers restore monetary-parameter discussion to an ordinary engineering question, or has the social-layer enforcement (community ostracism, funding structures) made the lock self-perpetuating regardless of who holds maintainer roles?',
    'Track whether newer contributors who did not personally build their reputations defending immutability show different openness to tail-emission or parameter-change proposals than long-tenured maintainers.',
    'If the lock is generational/personal, it may loosen naturally over decades. If it is structural (funding, community norms, holder-funded media) it will persist independent of who holds the roles, supporting the tangled_rope''s requires_active_enforcement characterization as durable rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether the identity-lock on core developers is personal or structurally self-perpetuating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bitc_tr_t0, observed).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t8, observed).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t16, observed).
narrative_ontology:measurement(bitc_tr_t24, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(bitc_tr_t24, observed).
narrative_ontology:measurement(bitc_tr_t32, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement_basis(bitc_tr_t32, observed).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(bitc_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(bitc_be_t0, observed).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(bitc_be_t8, observed).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(bitc_be_t16, observed).
narrative_ontology:measurement(bitc_be_t24, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(bitc_be_t24, observed).
narrative_ontology:measurement(bitc_be_t32, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(bitc_be_t32, observed).
narrative_ontology:measurement(bitc_be_t40, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(bitc_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bitc_su_t0, observed).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(bitc_su_t8, observed).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(bitc_su_t16, observed).
narrative_ontology:measurement(bitc_su_t24, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(bitc_su_t24, observed).
narrative_ontology:measurement(bitc_su_t32, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement_basis(bitc_su_t32, observed).
narrative_ontology:measurement(bitc_su_t40, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(bitc_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.05).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the Bitcoin whitepaper establishes immutable monetary policy.' Each sibling reads the same founding text and consensus history differently: maximalist_reading (this file, high epsilon, tangled_rope) treats the covenant as unappealable and authors a wealth-transfer structure toward early holders; utility_reading authors the whitepaper as a minimum-viable mechanism enabling iterative improvement (expected low epsilon, closer to rope); pragmatic_synthesis authors a layered structure where only base-layer monetary rules are immutable while upper layers permit innovation (expected moderate epsilon, narrower victim set). All three share the same underlying text and consensus-history facts but diverge sharply on beneficiary/victim structure and extraction because they diverge on what the founding covenant actually claims. Per the epsilon-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
