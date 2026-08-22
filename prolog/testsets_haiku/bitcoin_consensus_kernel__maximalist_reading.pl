% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Bitcoin Whitepaper Immutability Covenant (Maximalist Reading)
 *   domain: cryptoeconomics/monetary-systems/distributed-consensus
 *
 * SUMMARY:
 *   Bitcoin's whitepaper is treated by one influential reading as
 *   establishing immutable monetary rules that cannot be changed without
 *   violating the founders' covenant. Under this maximalist reading,
 *   proposals to increase throughput, add scripting capabilities, or modify
 *   consensus parameters are framed as heretical and destabilizing. This
 *   reading benefits early adopters, institutional holders, and mining
 *   interests by locking in the monetary base and preventing dilution or
 *   innovation-driven changes. It extracts from developers and users who wish
 *   to improve or scale the protocol. The reading is held by a powerful
 *   organizing coalition (whitepaper originalists, major hodlers, core dev
 *   stewards) who use narrative authority to block competing readings
 *   (pragmatic synthesis, utility-focused). The constraint is NOT presented
 *   as a choice; it appears as the *only* legitimate reading of the founding
 *   document. This is the maximalist reading instantiated as a single
 *   constraint story. The kernel contains three reading: this one
 *   (maximalist), plus pragmatic_synthesis and utility_reading (authored
 *   elsewhere).
 *
 * KEY AGENTS:
 *   - Bitcoin hodlers and early adopters (beneficiary; organized; arbitrage exit) — hold the token, benefit from immutability
 *   - Mining oligarchy (beneficiary; institutional; mobile exit) — control consensus, extract rents via block rewards and fees
 *   - Layer-2 developers (payer; moderate; constrained exit) — forced to build workarounds due to layer-1 throughput constraints
 *   - Scalability researchers (payer; moderate; mobile exit) — research paths blocked by immutability reading
 *   - Protocol upgrade proponents (payer; powerful; constrained exit) — blocked by narrative legitimacy deficits
 *   - Transaction users (payer; powerless; trapped exit) — face rising fees, cannot access layer-1 directly due to scalability limits
 *   - Core development stewards (agenda_setter; institutional; analytical exit) — set the reference implementation and narrative boundary
 *   - Whitepaper originalists (agenda_setter; organized; analytical exit) — maintain the immutability narrative and fend off competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.78).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.71).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Whitepaper Immutability Covenant (Maximalist Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomics/monetary-systems/distributed-consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '56b25229-7335-457e-8361-027e9b6536db').
narrative_ontology:cs_kernel_codification('56b25229-7335-457e-8361-027e9b6536db', fixed_text).
narrative_ontology:cs_authority_grounding('56b25229-7335-457e-8361-027e9b6536db', extraction).
narrative_ontology:cs_interpretation_layer_present('56b25229-7335-457e-8361-027e9b6536db').
narrative_ontology:cs_reading_relation('56b25229-7335-457e-8361-027e9b6536db', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_reading_relation('56b25229-7335-457e-8361-027e9b6536db', bitcoin_consensus_kernel__utility_reading, influences).
narrative_ontology:cs_axiom('56b25229-7335-457e-8361-027e9b6536db', foundational, whitepaper_establishes_immutable_rules).
narrative_ontology:cs_axiom_status(whitepaper_establishes_immutable_rules, holdable).
narrative_ontology:cs_axiom_grounding('56b25229-7335-457e-8361-027e9b6536db', whitepaper_establishes_immutable_rules, deontological).
narrative_ontology:cs_axiom('56b25229-7335-457e-8361-027e9b6536db', foundational, protocol_changes_violate_founding_covenant).
narrative_ontology:cs_axiom_status(protocol_changes_violate_founding_covenant, holdable).
narrative_ontology:cs_axiom_grounding('56b25229-7335-457e-8361-027e9b6536db', protocol_changes_violate_founding_covenant, deontological).
narrative_ontology:cs_axiom('56b25229-7335-457e-8361-027e9b6536db', secondary, immutability_is_non_negotiable).
narrative_ontology:cs_axiom_status(immutability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('56b25229-7335-457e-8361-027e9b6536db', immutability_is_non_negotiable, instrumental).
narrative_ontology:cs_reference_frame('56b25229-7335-457e-8361-027e9b6536db', whitepaper_constitutionalism).
narrative_ontology:cs_drift_state('56b25229-7335-457e-8361-027e9b6536db', contemporary_scaling_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56b25229-7335-457e-8361-027e9b6536db', '2026-06-19T00:00:00Z').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, bitcoin_hodlers_early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, mining_oligarchy).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, maximalist_narrative_custodians).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_innovation_developers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, protocol_upgrade_proponents).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, transaction_scalability_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, whitepaper_originalists).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, institutional_custodians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin and benefit from the immutability covenant: their holdings are protected by the permanent 21 million cap, no dilution, no surprise monetary expansion. They have access to arbitrage across jurisdictions and exchanges; their exit is choosing not to hold rather than forced exit. Narrative custodians among this group actively defend the immutability reading against protocol changes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, bitcoin_hodlers_early_adopters, beneficiary,
    organized, generational, arbitrage, global).

% Operate the consensus machinery and extract rents through block rewards and transaction fees. They benefit from the immutability covenant because protocol changes (especially those enabling layer-2 scaling or altering fee structures) threaten their revenue model. They have sufficient hash power to block unwanted changes through consensus mechanics. Can exit by redirecting hardware to other chains, but the sunk cost in Bitcoin-specific ASICs is substantial.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, mining_oligarchy, beneficiary,
    institutional, biographical, mobile, global).

% Develop layer-2 solutions (Lightning, Stacks, Rollups) to work around Bitcoin's limited throughput. They bear the constraint cost: their workarounds are architecturally expensive and lack the finality and security of layer-1 settlement. They cannot easily exit because the layer-1 protocol's immutability is treated as a given; building on a layer-1 they could reshape would carry execution risk from chain forks. Blocked from proposing certain layer-1 changes by the immutability reading.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_innovation_developers, payer,
    moderate, biographical, constrained, global).

% Research protocol improvements to increase transaction throughput, reduce latency, or improve energy efficiency. The immutability covenant blocks certain research paths (parameter changes, script extensions, consensus rule modifications) as violations of the whitepaper covenant. They have exit: publishing research on other chains or moving to Ethereum/alternative networks; but Bitcoin's network effect means rejected proposals have limited impact.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, scalability_researchers, excluded).

% Developers, institutions, and users who propose protocol changes (block size increases, script enhancements, new opcodes, etc.). The immutability reading blocks them politically and socially: proposing certain changes is framed as violating the founders' covenant, betraying the vision, or introducing systemic risk. They must either accept the constraint, fork the chain (costly, loses network effect), or mount political campaigns (resource-intensive, often unsuccessful). Exit via fork is technically possible but economically punitive.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, protocol_upgrade_proponents, payer,
    powerful, generational, constrained, global).

% End users who wish to transact on Bitcoin but face rising fees and confirmation delays as demand exceeds base-layer throughput. They are trapped by network effects: they need Bitcoin specifically (not an alternative chain) for store-of-value credibility or institutional compatibility. The immutability constraint forces them to layer-2 solutions, which introduce new risk, longer settlement times, or custodial intermediaries. Their voice is absent from consensus decisions; they exit only by leaving Bitcoin entirely, which costs them the credibility function they sought.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, transaction_scalability_users, payer,
    powerless, immediate, trapped, global).

% The developers, maintainers, and institutions (Blockstream, Square, etc.) who control the reference implementation and narrative around protocol design. They have gate power over which changes are merged into the reference client and which are rejected or shunted to alternative implementations. The immutability reading is a tool that legitimizes rejecting certain changes as 'violating the founders' intent.' They set the boundary of what counts as a valid proposal.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, core_development_stewards, agenda_setter,
    institutional, generational, analytical, global).

% An informal governing coalition that interprets the whitepaper as a binding constitutional document establishing permanent monetary rules. This group includes influential social media figures, protocol ideologues, and some major holders. They set the discourse boundary: proposals are evaluated against 'what Satoshi meant' and 'the original vision.' They benefit indirectly through narrative control and the preservation of Bitcoin's immutable mystique, which supports the store-of-value narrative that props up the token's valuation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, whitepaper_originalists, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__maximalist_reading, whitepaper_originalists, beneficiary).

% Ethereum, Solana, and other chains that permit faster innovation on base-layer protocols. They would benefit from Bitcoin developers and capital migrating to experiment with alternative monetary policies and scaling approaches. They are excluded from Bitcoin's consensus by the network's immutability enforcement; if Bitcoin developers migrated to chains with more flexible governance, Ethereum's network effects would strengthen. Their exclusion is structural, not by vote, but by the immutability reading's delegitimization of layer-1 experimentation.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, alternative_chain_ecosystem, excluded,
    powerful, biographical, trapped, global).

% BlackRock, Fidelity, nation-states, and institutional investors who adopt Bitcoin as a store of value. They benefit from the immutability covenant: it signals permanent monetary properties and a lack of capricious governance. The immutability reading protects their thesis that Bitcoin is 'digital gold' and not subject to the discretionary changes that could undermine the asset class. They have exit via portfolio reallocation, but the signal value of Bitcoin's immutability is central to their adoption narrative.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, institutional_custodians, beneficiary,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__maximalist_reading, bitcoin_hodlers_early_adopters).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a permanent, algorithmically-enforced monetary base layer: a global monetary system with a fixed supply, no central authority, and rules interpreted as inviolable. Coordinates on the belief that money should have immutable properties and that the founding whitepaper establishes those properties permanently.
% TRANSFER_FUNCTION: Extracts opportunity cost and innovation capacity from those who wish to change protocol parameters or layer-1 throughput: the constraint redistributes legitimacy and governance power toward holders and mining operators (who benefit from immutability) and away from developers and users (who bear the cost of constrained innovation).
% ABSENT_VOICES: Protocol upgrade proponents, scalability researchers, and transaction users are structurally excluded from governance: their preferences are not formally solicited; their objections are delegitimized as 'violating the covenant.' Alternative chain ecosystems would articulate a competing vision of permissioned innovation but are kept out by network effects and the immutability narrative's delegitimization of experimentation. The pragmatic synthesis and utility readings are live positions held by other factions but are pushed to lower-status positions in discourse and network upgrades.
% DISAPPEARANCE_RATIONALE: If the immutability covenant disappeared and the pragmatic synthesis reading won consensus acceptance, protocol changes would accelerate (throughput, new capabilities, energy efficiency improvements); mining incentive structures might shift; the store-of-value narrative would bifurcate (is Bitcoin still 'digital gold' if it changes?); institutional adoption thesis would recalculate; layer-2 development would deprioritize in favor of layer-1 innovation. The entire cryptoeconomic order built on Bitcoin's immutability signal would reorganize.
% FOUNDING_PROBLEM: Early cryptocurrencies faced governance capture and unexpected supply changes (e.g., early alt-coins suffered dilution, hard forks, developer-directed changes that broke the store-of-value property). Bitcoin's whitepaper promised to fix this: a protocol where the monetary rules are permanent, verifiable, and not subject to governance override. The immutability covenant was built to credibly commit that Bitcoin's monetary properties could never be unilaterally changed.
% FOUNDING_PROBLEM_CORROBORATION: The maximalist reading asserts the founding problem (the need for immutable monetary rules) is still live. However, competing readings argue the problem is substantially solved by other mechanisms: layer-2 protocols isolate users from protocol changes; institutional adoption and global distribution create de facto immutability through coordination costs rather than formal rules; the pragmatic synthesis reading argues the founding problem applies only to the monetary base, not to layer-1 scalability. Independent academic cryptography and game theory literature supports the pragmatic synthesis reading: that immutability of the monetary base does not require immutability of throughput or script capabilities. The maximalist reading's corroboration comes primarily from holders and mining interests, not from sources outside the benefiting parties. Ethereum's successful base-layer upgrades (PoS transition, Shanghai, Dencun) without supply dilution or governance capture provide counter-evidence: the founding problem may have been solved by mechanisms other than immutability of all layer-1 parameters.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) because the immutability covenant extracts innovation capacity, throughput gains, and governance authority from protocol-change advocates. The constraint is coordinating (it establishes stable monetary rules), but the coordination accrues almost entirely to holders; the extraction is the price imposed on innovators and users. Suppression is substantial (0.71) because blocking change requires active enforcement: hash power to reject soft forks, narrative authority to delegitimize proposals, and stewards to maintain the reference client. The theater_ratio (0.42) indicates that a significant portion of the suppressive effort is theatrical: the 'immutable covenant' framing is invoked repeatedly even when the technical mechanism is straightforward consensus signaling. Rising measurements over the interval reflect the increasing resource investment in maintaining the immutability narrative as competing readings gained traction (pragmatic synthesis, scaling research on Ethereum). The accessibility_collapse (0.68) is moderate because alternatives exist (layer-2, other chains) but carry high opportunity cost due to network effects. Resistance (0.72) is substantial because scalability researchers, layer-2 teams, and institutional users (institutions that want to use Bitcoin but need throughput) actively push back against the constraint. This is NOT a natural law that emerges without opposition.
 *
 * PERSPECTIVAL GAP:
 *   The maximalist and pragmatic synthesis readings compute different seats with different d values. From the maximalist seat (beneficiary): the constraint is a coordination solution that ensures Bitcoin remains a credible store of value. From the pragmatic synthesis seat (protocol innovator): the same constraint is extraction — the monetary base could be immutable while layer-1 throughput improved. The engine should compute this divergence: beneficiary seats should register lower effective extraction (coordination is real, the cost is background noise); target seats should register higher extraction (the constraint is actively suppressive). This gap is the measurement the corpus should capture: do the metrics and structural data permit the engine to compute that same-constraint, different-seats yields different per-seat types?
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (hodlers, mining oligarchy, originalist custodians) derive full beneficiary status: they collect from the constraint (asset protection, narrative authority, fee extraction) and have high exit options (can always sell, can move to other chains while maintaining store-of-value positioning). Their directionality sits near 0.1–0.2. The payers (scalability users, developers, researchers) derive target status: they bear extraction (blocked innovation, forced layer-2 workarounds, narrative delegitimization) and have constrained exit (cannot substitute Bitcoin for its network effects). Their directionality sits near 0.85–0.95. The core developers and originalist custodians sit between: they set the agenda (institutional power, narrative authority) but also bear identity costs if the immutability covenant fractures — their professional identity is fused with the reading. This constraint should compute differently per seat: from the hodler seat, it is genuine coordination with side benefits; from the developer seat, it is enforced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for immutable monetary rules to prevent governance capture) was genuinely live at Bitcoin's founding. The maximalist reading certifies this problem as still live. However, the pragmatic synthesis and utility readings argue the founding problem is substantially solved by mechanisms other than layer-1 immutability: (a) institutional adoption and geographic distribution make unilateral changes politically infeasible; (b) layer-2 protocols isolate users from most layer-1 changes; (c) Bitcoin's network effect is so large that casual changes would trigger forks, creating exit costs for any developer reckless enough to attempt dilution. The measuring question (mandatrophy resolution): is the measured suppression (0.71) needed to hold the immutability constraint, or is it theater protecting a constraint that would persist anyway? If the latter, the founding problem is dead and the constraint is a zombie holding office via narrative. If the former, the problem is live. The measurement series shows rising theater_ratio and suppression_requirement over the interval, which suggests increasing resource investment in narrative and enforcement — a sign that the constraint's self-evidence is declining. This pattern is consistent with a dead founding problem that requires theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_vs_pragmatic_evolution,
    'Is the whitepaper a constitutional covenant establishing permanent monetary rules, or a minimal consensus specification enabling iterative improvement on non-monetary layers?',
    'Textual analysis of Satoshi''s original white paper and early writings, combined with game-theoretic analysis of whether immutability of the monetary base requires immutability of throughput/scripting layer. Comparison with actual protocol evolution: which changes have been accepted historically, and what principle determined acceptance?',
    'If the whitepaper is a constitutional covenant, the maximalist reading classifies as tangled_rope (coordination + extraction enforced by ideological consensus). If the whitepaper is a minimal specification, the constraint reclassifies to snare (pure extraction using the covenant narrative as cover); the founding_problem_status shifts to dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_pragmatic_evolution, conceptual, 'The fundamental interpretive contest: is the whitepaper immutable or enabling?').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.71) structural (hash power/code fork threats) or internalized (developers and users accept the immutability narrative as legitimate)?',
    'Post-fork analysis: if a soft fork attempting layer-1 throughput improvement gains adoption despite maximalist opposition, suppression was structural (reversible via fork coordination); if the fork fails despite technical feasibility, suppression is partially internalized (the narrative has authority). Survey of developers on exit costs: do they leave because exit is technically blocked, or because the immutability narrative delegitimizes change?',
    'If suppression is purely structural, the constraint is more fragile (forks could dissolve it). If partially internalized, the constraint is more durable; the measured suppression understates the effective binding force. Implications for type stability: a constraint held by internalized suppression is more robust to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the immutability constraint''s enforcement is architectural or ideological.').

omega_variable(
    kernel_reading_genealogy_satoshi_intent,
    'Is the immutability reading authentic to Satoshi''s original intent, or a post-hoc narrative constructed by holders to justify regulatory defensibility and property protection?',
    'Satoshi''s emails and early forum posts; the evolution of Bitcoin discourse from 2008–2015 (pre-scaling crisis) to post-2015 (scaling wars); identification of when the ''whitepaper as constitution'' framing emerged as an explicit rhetorical strategy.',
    'If immutability is authentic to founding intent, the maximalist reading is grounded in genuine kernel-level fidelity. If the immutability narrative is post-hoc, the constraint is revealed as a false-summit mountain: it presents as natural law (the whitepaper is immutable) but was constructed to benefit holders. This affects the mandatrophy analysis: an authentic founding problem that remains live vs. a defunct problem masked by narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_genealogy_satoshi_intent, conceptual, 'The genealogy of the immutability reading: authentic founding principle or post-hoc narrative?').

omega_variable(
    network_effect_lock_vs_narrative_constraint,
    'Are developers and users trapped by network effects (genuine exit cost), or constrained only by the immutability narrative (exit is costless if they reject the reading)?',
    'Observe actual exit behavior: when developers fork or migrate to alternative chains, what are their stated reasons? Do they cite technical inability or narrative delegitimization? Are alternative chains growing at rates that suggest viable substitution, or network effects remain dominant?',
    'If network effects are the binding constraint, suppression metrics capture the true cost. If the immutability narrative is the binding constraint, the payers have higher agency than the metrics suggest; the constraint is more fragile to coordinated narrative shifts (e.g., if major developers publicly rejected the immutability reading, could they shift consensus?). Affects classification: if narrative-bound, the constraint edges toward snare; if network-bound, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_vs_narrative_constraint, empirical, 'What actually binds the payers: the immutability reading or network effects?').

omega_variable(
    kernel_reading_contest_scope_ambiguity,
    'Does the immutability covenant apply only to the 21 million cap and difficulty adjustment, or to the entire consensus rule set including throughput, opcodes, and script capabilities?',
    'Textual analysis of the whitepaper: what does it actually specify as immutable? Technical history: which changes have been attempted and which blocked? Discourse analysis: when holders invoke the covenant, what specifically are they defending?',
    'If the covenant applies narrowly (only to monetary base), the maximalist and pragmatic readings differ only on emphasis and are more easily compatible — both hold the monetary base immutable. If the covenant applies broadly, they genuinely foreclose each other: one reading permits base-layer innovation (pragmatic) and the other forbids it (maximalist). This determines the reading_relations value: coexists_with or forecloses?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_scope_ambiguity, conceptual, 'The scope ambiguity of the immutability covenant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(bitc_tr_t18, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement(bitc_tr_t24, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.66).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(bitc_be_t18, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement(bitc_be_t24, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(bitc_su_t18, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(bitc_su_t24, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__maximalist_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bitcoin consensus kernel. The kernel admits three structurally distinct readings: maximalist (this story), pragmatic_synthesis (permits layer-1 innovation while preserving monetary base immutability), and utility_reading (treats immutability as pragmatic feature, not constitutional principle). Each reading has a different beneficiary/victim structure, different ε, and different founding_problem_status. The ε for this reading (0.78) reflects high extraction against protocol changes; the pragmatic_synthesis reading permits some layer-1 change and would have lower ε for the same innovation proposals. See cs_structure.reading_relations and cs_structure.axioms for formal structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__maximalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
