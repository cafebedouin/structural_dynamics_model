% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: Nakamoto Oracle Vacuum — Whitepaper as Contested Substrate
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Satoshi Nakamoto withdrew from all public Bitcoin communication by early
 *   2011 and never returned, eliminating the only party with standing to
 *   authoritatively interpret the whitepaper. What remains is a fixed
 *   nine-page text with no authoritative interpreter: every faction in every
 *   subsequent protocol dispute — block size, fees, purpose — has claimed
 *   whitepaper fidelity, and none can be refuted by appeal to founder intent.
 *   This story instantiates the nakamoto_oracle_opacity reading of the kernel
 *   bitcoin_whitepaper_purpose: the operative constraint is not any
 *   particular telos but the interpretive vacuum itself, which enables fork
 *   proliferation (each fork claims equal fidelity), lets both sibling
 *   readings persist as live positions, and provides no mechanism for
 *   convergence short of founder clarification. The claim/metric gap is
 *   deliberate per corpus rules: the constraint is CLAIMED as a hybrid — a
 *   genuine anti-capture coordination function wrapped around asymmetric
 *   extraction — while the metrics describe the vacuum's actual operation;
 *   the engine measures the divergence. Epsilon's referent is the standing
 *   arrangement under contest (the no-oracle settlement), assessed by this
 *   reading's lights: the vacuum is substantially costly — fifteen years of
 *   fork wars, replay losses, and unresolved purpose — while still delivering
 *   the capture-resistance the design intended. KEY AGENTS (by structural
 *   relationship): see key_agents.
 *
 * KEY AGENTS:
 *   - satoshi_nakamoto: Absent founder (powerful/trapped) — the only potential authoritative interpreter, structurally removed from the conversation; his silence is the constraint's operating condition
 *   - bitcoin_core_developers: De facto agenda-setter (organized/identity_locked) — administers the reference implementation and proposal review; collects agenda control, bears usurpation attacks
 *   - fork_proponents: Beneficiary (organized/mobile) — claim equal whitepaper fidelity for rival chains; forking is both strategy and exit
 *   - large_mining_pools: Beneficiary with payer burden (organized/mobile) — hash weight settles what the absent author cannot; arbitrage position across competing chains
 *   - bitcoin_users: Primary target (moderate/constrained) — bear fork costs, replay losses, fee volatility
 *   - merchants_payment_processors: Target (moderate/constrained) — need predictable rails; absorbed the capacity dispute's costs
 *   - oracle_claimants: Excluded (organized/identity_locked) — claim the founder's mantle or a privileged reading; delegitimized by the enforcement machinery
 *   - monetary_historians: Analytical observer (analytical/analytical) — study the episode without holding a fidelity position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Nakamoto Oracle Vacuum — Whitepaper as Contested Substrate").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '7cd3b08d-6de9-4600-ac13-5c3e767ab0ee').
narrative_ontology:cs_kernel_codification('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', fixed_text).
narrative_ontology:cs_authority_grounding('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', lineage).
narrative_ontology:cs_reading_relation('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', bitcoin_whitepaper_purpose__store_of_value_reading, influences).
narrative_ontology:cs_reading_relation('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_axiom('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', foundational, no_living_authoritative_interpreter).
narrative_ontology:cs_axiom_status(no_living_authoritative_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', no_living_authoritative_interpreter, empirically_contingent).
narrative_ontology:cs_axiom('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', secondary, consensus_power_adjudicates_kernel_disputes).
narrative_ontology:cs_axiom_status(consensus_power_adjudicates_kernel_disputes, holdable).
narrative_ontology:cs_axiom_grounding('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', consensus_power_adjudicates_kernel_disputes, conventional).
narrative_ontology:cs_reference_frame('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', founder_anchored_kernel_text).
narrative_ontology:cs_drift_state('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', post_2011_founder_withdrawal, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7cd3b08d-6de9-4600-ac13-5c3e767ab0ee', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_core_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_proponents).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_mining_pools).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchants_payment_processors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_core_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_mining_pools).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, decentralized_consensus_legitimacy).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, leaderless_governance_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the whitepaper and original implementation, then withdrew from all public communication by 2011. His early writings are the only candidate for a definitive reading of the text, and every faction cites them. Return is practically foreclosed: a verified reappearance would either be disbelieved — the community has already litigated and rejected one such claim — or would shatter the leaderless legitimacy the ecosystem has since built around his absence. The one voice that could settle the text's meaning therefore stays structurally outside the conversation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto, excluded,
    powerful, generational, trapped, global).

% Maintain the reference implementation and run the proposal-review process. With no founder to interpret the text, their merge decisions have become the nearest thing to interpretation, so every protocol dispute routes through them. They gain agenda control over what gets built, but hold no formal interpretive mandate: each contested decision draws accusations of usurpation from some faction, and their working identities are fused with the project — stepping back would hand stewardship to rivals they believe would break the system.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_core_developers, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_core_developers, payer).

% Launched and operate alternative chains that cite the same whitepaper; the unresolvable text lets each fork claim equal fidelity to the author's intent. Forking is both their strategy and their exit: they can leave the main ledger at will, taking a copy of the transaction history with them, and they collect chain legitimacy, media standing, and market value from the text's openness.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, fork_proponents, beneficiary,
    organized, biographical, mobile, global).

% Operate the hash power that settles what the absent author leaves unsettled: when interpretations conflict, whichever chain attracts majority hash weight tends to retain the name and the network. They can redirect hash between competing chains at will, an arbitrage position in every dispute, though fork wars also destroy coin holdings and equipment returns they carry on their books.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_mining_pools, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_mining_pools, payer).

% Hold and transact in the asset whose meaning is contested. When interpretive disputes escalate to chain splits they absorb replay-attack losses, split-management work, and fee volatility, and they must guess which chain will keep the name and the network effects. Individual exit — selling out or moving to another asset — is possible but costly in custody arrangements, technical knowledge, and payment-network access.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_users, payer,
    moderate, biographical, constrained, global).

% Built payment rails on the expectation of one stable protocol. The unresolved text gave them no definitive answer on fee levels or confirmation times during the 2015-2017 capacity dispute, and they absorbed the resulting fee spikes and settlement delays. Many diversified into other assets or left rather than keep betting on an unsettled document; their horizon is operational — predictable rails now, not eventual clarity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchants_payment_processors, payer,
    moderate, immediate, constrained, global).

% Actors who claim the founder's mantle or a privileged reading of the text — most prominently the claimant who asserted for roughly a decade that he wrote the whitepaper and litigated for recognition, including forcing the document's temporary removal from a major project website in 2021. The community's enforcement machinery exists in large part to reject such claims: they are heard, then delegitimized, delisted, and sued. Their projects' identities are fused with the claim, so retreat would dissolve the projects themselves.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, oracle_claimants, excluded,
    organized, biographical, identity_locked, global).

% Study the episode as a case in monetary and institutional history: a founder who withdrew, a canonical text left without an interpreter, and a community that made leaderlessness a legitimacy requirement. They take no side in the fidelity contest and bear none of its costs; their seat is analytical.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_core_developers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The vacuum coordinates protocol legitimacy by removing every candidate for interpretive capture: no founder, foundation, or council exists to be pressured, subpoenaed, or bought into redefining the system, so disputes over the text's meaning can only be settled by verifiable network consensus — hash weight, node enforcement, economic majority — and any faction can test its reading by forking and letting the market judge.
% TRANSFER_FUNCTION: Moves interpretive authority from a single author to whoever can mobilize network consensus; moves de facto agenda control over protocol direction to the reference-implementation maintainers; and moves the costs of unresolved interpretation — chain splits, replay losses, fee volatility — onto users and merchants, while legitimacy rents accrue to factions that successfully claim whitepaper fidelity.
% ABSENT_VOICES: The founder himself is the paradigmatic absent voice: the only party whose word could settle the text, present only as citations every faction weaponizes. Merchants who wanted the electronic-cash direction largely lost the 2017 contest and left the conversation; ordinary users have no formal seat in the interpretive contest at all; and oracle claimants are heard only to be delegitimized.
% DISAPPEARANCE_RATIONALE: If the vacuum vanished overnight — say, the founder verifiably returned and issued a binding interpretation — fork proliferation would collapse, one reading would be ratified and the others would lose their fidelity claims, the maintainers' de facto authority would be subordinated to the restored oracle, and the ecosystem's entire legitimacy structure would reorganize around restored interpretive authority. The no-oracle condition is load-bearing for the whole governance settlement.
% FOUNDING_PROBLEM: The system was designed to function without trusted leadership: the author's stepwise 2010-2011 withdrawal — handing off maintainership, removing his name from project communications, declining dispute-settlement requests — transferred interpretation from the author to the network, so that no future founder could be coerced, corrupted, or captured into redefining the protocol. The vacuum is the intended end-state of that design, not an accident.
% FOUNDING_PROBLEM_CORROBORATION: The author's own 2010-2011 correspondence and forum posts attest the leaderless handoff was deliberate; documented cypherpunk-movement ideology — outside the current beneficiary set — attests the capture-resistance design goal; and regulators' documented inability to identify any single actor who can unilaterally change the protocol corroborates, from outside the benefiting parties, that the capture-resistance function remains live. No party inside the ecosystem disputes that the vacuum was intended; the contest is over whether its costs now exceed its protection.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not dominant: the vacuum's costs are real and recurring — the 2015-2017 capacity dispute escalated to a chain split because no interpretive authority could rule on the block-size question, users absorbed replay attacks and split-management losses, and fee volatility drove merchants out — yet the same arrangement still performs its designed function of keeping protocol definition uncapturable, which caps how extractive it can become without losing the beneficiary base that defends it. Suppression (0.62) is social and economic rather than legal: the enforcement machinery maintaining the vacuum includes delegitimization campaigns against oracle claimants, exchange delistings of claimant-affiliated chains, litigation defense, and the cultural foreclosure of every attempted interpretive institution — the Bitcoin Foundation's authority ambitions collapsed within roughly two years. Theater (0.45) is high because a large share of interpretive activity is performative fidelity-claiming — whitepaper-quotation battles, 'Satoshi's vision' rhetoric, the 2021 episode in which the whitepaper was briefly pulled from a major project website under copyright threat — rather than engineering, though the consensus machinery does real adjudicative work underneath. Accessibility collapse (0.55): inside Bitcoin's culture the alternative to the vacuum (an authoritative interpreter) collapses on contact — every attempt is rejected — but ecosystem-level exit via fork remains open, so alternatives are partly, not completely, collapsed. Resistance (0.6): oracle claimants, governance proposals, and vision movements have contested the vacuum continuously since roughly 2014. Time points map 2011 to 2026 in three-year steps (t=0 is the withdrawal; t=6 is the 2017 fork war; t=15 is the present). All three tracked series share this one grid; extractiveness peaks at the fork war and decays as the acute phase passes while the structural contest persists, and suppression_requirement rises monotonically because the enforcement machinery for rejecting oracle claims matured and hardened over the interval.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp. From the maintainer seat, the vacuum is an uninvited burden: they never sought interpretive authority, yet every protocol decision is attacked as usurpation, and they cannot delegate a role they never claimed. From the fork-proponent seat, the same vacuum is an opportunity structure: an unfalsifiable fidelity claim plus a fork option equals a chain-launch kit. From the user seat, it is unresolved risk priced into every custody decision — which chain will keep the name, which will replay-attack them. From the miner seat, it is an arbitrage surface. One arrangement, four experienced realities; the engine computes the divergence from the structural data rather than the authored claim adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: fork proponents collect legitimacy rents with mobile exit, placing them near the beneficiary end; large mining pools collect arbitral power plus cross-chain arbitrage, likewise near the beneficiary end; bitcoin_core_developers are listed beneficiaries because the vacuum routes agenda control to their review process, but their situation includes heavy offsetting burdens — usurpation attacks, no formal authority, identity lock — placing them nearer the symmetric midpoint than the raw beneficiary listing implies. Victims: bitcoin_users bear fork costs with constrained exit, placing them near the target end; merchants_payment_processors bear operational costs with constrained exit, likewise near the target end. Satoshi Nakamoto is the excluded seat: the constraint is his absence, and his directionality is structurally undefined — commentary-grade only, never correction-grade. The directionality_overrides array is deliberately empty: the schema keys overrides by power atom rather than by agent, and an 'organized'-atom override calibrated for the maintainer seat would also capture fork proponents and miners, whose derived directionality is already correct; the maintainer-seat ambiguity is therefore routed to the maintainer_directionality_ambiguity omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping protocol definition uncapturable — is live, so this is not a mandatrophy case: the vacuum still performs the work it was designed for. The classification guards against two mislabels. First, the natural-limit mislabel ('the founder is gone; nothing can be done'): the vacuum is partly a maintained design choice with identifiable beneficiaries, which is why beneficiaries are declared and the vacuum_naturalness_ambiguity omega is carried. Second, the pure-extraction mislabel: the vacuum's anti-capture function is genuine coordination, not cover — it is precisely why the ecosystem defends leaderlessness even at high cost — and an extraction-only framing would erase the reason the arrangement persists. The hybrid classification keeps both faces legible. The R5 pairing (founding_problem_status live x disappearance_verdict world_rearranges) shows no mismatch: no dead-mandate flag, no zombie arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'This story is one reading of kernel bitcoin_whitepaper_purpose (reading: nakamoto_oracle_opacity). Are the sibling readings'' telos claims — electronic cash versus store of value — the operative constraints, with the interpretive vacuum merely the condition that keeps them unresolved? Or is the vacuum itself the operative constraint, as this reading holds, with the telos dispute being a downstream symptom?',
    'A verifiable founder clarification, or a legitimacy event in which one reading wins uncontested social consensus, would adjudicate the siblings and reveal whether the vacuum or a telos was doing the binding.',
    'If a sibling reading is operative, this story''s epsilon and victim set are misattributed to the vacuum, and the family''s classifications restructure around the ratified telos; if this reading is operative, the sibling stories are downstream manifestations of the vacuum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Which element of the kernel contest binds: the interpretive vacuum or one of the telos readings.').

omega_variable(
    vacuum_naturalness_ambiguity,
    'Is the no-oracle condition an irreducible historical fact — the founder is gone and his intent is unrecoverable — or an actively maintained design choice that the community could reverse by constructing interpretive institutions?',
    'Compare governance trajectories across comparable projects: communities that built interpretive mechanisms (foundations, councils, on-chain governance) versus Bitcoin''s repeated refusal of them (the Bitcoin Foundation''s collapse as an authority, rejection of every formal-interpretation proposal). If workable mechanisms are actively refused, the vacuum is maintained rather than natural.',
    'If maintained, the constraint is an enforced hybrid with identifiable beneficiaries (as claimed here); if irreducible, it trends toward a natural-limit profile and the enforcement metrics are misdescribed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vacuum_naturalness_ambiguity, empirical, 'Whether the interpretive vacuum is a natural fact or an enforced construction.').

omega_variable(
    maintainer_directionality_ambiguity,
    'Do reference-implementation maintainers net-benefit from the vacuum — de facto interpretive authority and agenda control accrue to them — or net-pay, absorbing usurpation attacks and legitimacy demands with no formal authority to match?',
    'Revealed-preference evidence: do maintainers attempt to shed the role, refuse protocol-direction decisions, or seek formal interpretive mandate? Welfare comparison against a counterfactual ecosystem with formal interpretive institutions.',
    'Flips the maintainer seat between the beneficiary end and the target end of the directionality scale, changing their computed per-seat classification and the capture analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_directionality_ambiguity, empirical, 'Whether maintainers are net beneficiaries or net targets of the vacuum.').

omega_variable(
    fork_proliferation_cost_or_right,
    'Is fork proliferation an extraction cost imposed on users (chain splits, replay attacks, name confusion, fee volatility) or an exit right that protects holders from unwanted protocol changes — and in what proportion?',
    'Audit realized fork events: measure user losses (replay incidents, split-management costs, fee-crisis damages) against cases where fork exit protected users from changes a supermajority rejected.',
    'If exit-right dominates, the vacuum''s extraction component is overstated and the constraint trends toward pure coordination; if cost dominates, it trends toward pure extraction with the coordination story as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fork_proliferation_cost_or_right, conceptual, 'Fork proliferation as imposed cost versus protected exit right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nakamoto_vacuum_tr_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t0, observed).
narrative_ontology:measurement(nakamoto_vacuum_tr_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t3, observed).
narrative_ontology:measurement(nakamoto_vacuum_tr_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 6, 0.55).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t6, observed).
narrative_ontology:measurement(nakamoto_vacuum_tr_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 9, 0.5).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t9, observed).
narrative_ontology:measurement(nakamoto_vacuum_tr_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 12, 0.55).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t12, observed).
narrative_ontology:measurement(nakamoto_vacuum_tr_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(nakamoto_vacuum_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(nakamoto_vacuum_be_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t0, observed).
narrative_ontology:measurement(nakamoto_vacuum_be_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 3, 0.44).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t3, observed).
narrative_ontology:measurement(nakamoto_vacuum_be_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 6, 0.66).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t6, observed).
narrative_ontology:measurement(nakamoto_vacuum_be_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 9, 0.62).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t9, observed).
narrative_ontology:measurement(nakamoto_vacuum_be_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t12, observed).
narrative_ontology:measurement(nakamoto_vacuum_be_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(nakamoto_vacuum_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(nakamoto_vacuum_su_t0, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t0, observed).
narrative_ontology:measurement(nakamoto_vacuum_su_t3, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 3, 0.3).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t3, observed).
narrative_ontology:measurement(nakamoto_vacuum_su_t6, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t6, observed).
narrative_ontology:measurement(nakamoto_vacuum_su_t9, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 9, 0.55).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t9, observed).
narrative_ontology:measurement(nakamoto_vacuum_su_t12, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t12, observed).
narrative_ontology:measurement(nakamoto_vacuum_su_t15, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(nakamoto_vacuum_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_block_size_cap).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Bitcoin whitepaper's purpose' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the electronic-cash reading (telos: low-fee transactional cash — its own story, its own epsilon and victim set), the store-of-value reading (telos: decentralization and full-node verifiability subordinate on-chain capacity — its own story), and this story, the nakamoto_oracle_opacity reading, which holds the operative constraint is the absence of authoritative interpretation. The vacuum is upstream of both siblings: because no oracle exists to adjudicate the telos dispute, both sibling readings persist as live fidelity claims and manifest as forks rather than converging; this story links both siblings and the downstream block-size-cap contest, where the vacuum first manifested as a full governance crisis (2015-2017).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
