% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Nakamoto Oracle Opacity — Interpretive Vacuum After Founder Disappearance
 *   domain: technology_governance/distributed_systems/monetary_theory
 *
 * SUMMARY:
 *   Satoshi Nakamoto's final public communication in April 2011 and
 *   subsequent disappearance created an interpretive vacuum around the
 *   Bitcoin whitepaper. The whitepaper — a nine-page technical document —
 *   became a contested kernel: multiple readings claim fidelity to its text
 *   while deriving mutually incompatible design imperatives. The 'electronic
 *   cash' reading binds to the title's 'cash' telos and demands on-chain
 *   transactional capacity. The 'store of value' reading binds to
 *   decentralization and full-node verifiability as supreme constraints. The
 *   'oracle opacity' reading (this constraint) identifies the structural
 *   condition itself: the founder's absence eliminated the only mechanism for
 *   authoritative interpretation, making the whitepaper a Rorschach
 *   substrate. No convergence mechanism exists without founder clarification.
 *   Fork proliferation (BTC/BCH/BSV, SegWit/Taproot activation conflicts,
 *   block size wars) is the direct structural consequence. The constraint
 *   extracts from all parties — developers pay coordination costs, users pay
 *   fork risk, merchants pay semantic instability — while coordinating only
 *   negatively (preventing any single capture).
 *
 * KEY AGENTS:
 *   - core_developers: Primary beneficiary (institutional/analytical) — control reference implementation, gatekeep consensus changes, accrue status/career capital from interpretive authority
 *   - mining_pools: Beneficiary (organized/powerful) — hashpower veto gives de facto interpretive leverage; extract value from fork optionality
 *   - institutional_holders: Beneficiary (organized/powerful) — narrative flexibility lets them position Bitcoin as whatever asset class suits current allocation thesis
 *   - application_layer_developers: Victim (moderate/powerless) — build on unstable semantics; stranded by forks; pay retooling costs
 *   - retail_users: Victim (powerless/moderate) — bear fork confusion, custody risk, fee volatility from unresolved scaling
 *   - merchant_adopters: Victim (moderate/powerless) — need stable payment semantics; driven away by fee unpredictability and fork risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Nakamoto Oracle Opacity — Interpretive Vacuum After Founder Disappearance").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "technology_governance/distributed_systems/monetary_theory").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'bbf1785e-8585-4777-bd83-5d6d8d7534d9').
narrative_ontology:cs_kernel_codification('bbf1785e-8585-4777-bd83-5d6d8d7534d9', fixed_text).
narrative_ontology:cs_authority_grounding('bbf1785e-8585-4777-bd83-5d6d8d7534d9', distributed).
narrative_ontology:cs_reading_relation('bbf1785e-8585-4777-bd83-5d6d8d7534d9', bitcoin_whitepaper_purpose__electronic_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbf1785e-8585-4777-bd83-5d6d8d7534d9', bitcoin_whitepaper_purpose__store_of_value_reading, coexists_with).
narrative_ontology:cs_axiom('bbf1785e-8585-4777-bd83-5d6d8d7534d9', foundational, founder_absence_as_permanent_governance_void).
narrative_ontology:cs_axiom_status(founder_absence_as_permanent_governance_void, holdable).
narrative_ontology:cs_axiom_grounding('bbf1785e-8585-4777-bd83-5d6d8d7534d9', founder_absence_as_permanent_governance_void, empirically_contingent).
narrative_ontology:cs_axiom('bbf1785e-8585-4777-bd83-5d6d8d7534d9', foundational, whitepaper_text_underdetermines_scaling_roadmap).
narrative_ontology:cs_axiom_status(whitepaper_text_underdetermines_scaling_roadmap, holdable).
narrative_ontology:cs_axiom_grounding('bbf1785e-8585-4777-bd83-5d6d8d7534d9', whitepaper_text_underdetermines_scaling_roadmap, empirically_contingent).
narrative_ontology:cs_reference_frame('bbf1785e-8585-4777-bd83-5d6d8d7534d9', nakamoto_active_guidance_era).
narrative_ontology:cs_drift_state('bbf1785e-8585-4777-bd83-5d6d8d7534d9', post_taproot_activation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bbf1785e-8585-4777-bd83-5d6d8d7534d9', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, mining_pools).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, application_layer_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchant_adopters).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, decentralization_as_supreme_value).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, code_is_law_immutability).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, founder_absence_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Bitcoin Core reference implementation. Gatekeep consensus changes through BIP process and merge permissions. Accrue professional status, consulting revenue, and grant funding from interpretive authority. Cannot easily exit — career capital and identity are bound to Bitcoin Core stewardship. Their reading of the whitepaper becomes the de facto standard through code control.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_developers, beneficiary,
    institutional, biographical, constrained, global).

% Control hashpower, which is the ultimate enforcement mechanism for consensus changes. Signal support for or against upgrades (SegWit, Taproot) based on revenue impact. Extract optionality value from fork risk — can mine either chain. Capital-intensive operations create high exit barriers. Their interpretive leverage comes from veto power, not textual fidelity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, mining_pools, beneficiary,
    organized, biographical, constrained, global).

% Hold Bitcoin as treasury asset (MicroStrategy, ETFs, nation-states). Benefit from narrative flexibility: can position Bitcoin as 'digital gold,' 'inflation hedge,' 'censorship-resistant money,' or 'institutional adoption play' depending on allocation thesis. Exit is liquid — can rotate to other assets. No technical commitment to any reading; they capture value from the vacuum's ambiguity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional_holders, beneficiary,
    organized, biographical, arbitrage, global).

% Build wallets, payment processors, exchanges, and applications on Bitcoin. Pay recurring costs: fork monitoring, chain-split handling, replay protection, user support during upgrades, retooling when semantics shift (e.g., SegWit address formats, Taproot script paths). Stranded by hard forks (BCH/BSV) that split user bases. Exit to other chains (Ethereum, Solana, L2s) carries rebuilding costs and user migration risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, application_layer_developers, payer,
    moderate, biographical, constrained, global).

% Hold and transact in Bitcoin. Bear fork confusion (which chain is 'real'?), custody risk during splits, fee volatility from unresolved block space markets. No interpretive voice — they experience the vacuum as unpredictability. Exit to other cryptocurrencies or fiat carries switching costs (learning curve, tax events, network effects). Most exposed to semantic instability with least leverage.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_users, payer,
    powerless, immediate, constrained, global).

% Accept Bitcoin as payment. Need stable semantics: predictable fees, fast confirmation, no chain-split payment finality risk. Driven away by fee spikes (2017, 2021) and fork uncertainty. Investment in Bitcoin-specific payment infrastructure (POS integration, accounting, compliance) creates trap — switching to another rail means sunk cost write-off. No voice in governance; experience the vacuum as business risk.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, merchant_adopters, payer,
    moderate, biographical, trapped, global).

% Study Bitcoin governance, consensus mechanisms, and fork dynamics from academic/analytical distance. No financial stake in any reading. Provide the external reference frame for evaluating whether the vacuum coordinates or extracts. Their analysis feeds into the engine's measurement of this constraint.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single party (foundation, corporation, state) from unilaterally defining 'Bitcoin' and changing its rules. The vacuum is a negative coordination mechanism: it coordinates by *preventing* centralized capture, not by *enabling* convergent development.
% TRANSFER_FUNCTION: Moves upgrade coordination costs, fork monitoring burden, application retooling expenses, and semantic instability risk from the beneficiary seats (core developers, miners, institutions) to the payer seats (app developers, users, merchants). The vacuum itself is the transfer mechanism — no one pays to maintain it, but everyone pays to navigate it.
% ABSENT_VOICES: Future users and developers who would build on a stable Bitcoin substrate but are deterred by semantic instability. Small-nation adopters (El Salvador-style) who need payment rails, not store-of-value narratives. Regulatory bodies seeking a stable definition for legal classification. These voices are absent because the vacuum has no representation mechanism — only hashpower and code control count.
% DISAPPEARANCE_RATIONALE: If the interpretive vacuum disappeared overnight (e.g., Nakamoto returned with authoritative interpretation, or a governance mechanism was established), the block size wars would resolve, fork proliferation would cease, application-layer development would accelerate on stable semantics, and merchant adoption would face predictable fee structures. The mobile software economy around Bitcoin would reorganize around whichever reading won — or a new synthesis.
% FOUNDING_PROBLEM: The whitepaper solved double-spending without trusted third parties via proof-of-work consensus. The interpretive vacuum was not a solution — it was the contingent consequence of the founder's disappearance. The founding problem (censorship-resistant digital cash) is live but the vacuum is not its solution; the vacuum is the *absence* of a mechanism to adapt the solution to new conditions (scaling, regulation, institutional adoption).
% FOUNDING_PROBLEM_CORROBORATION: Core developers and institutional holders attest the vacuum *is* the solution (prevents governance capture). Application developers, merchants, and academic researchers attest the founding problem (usable electronic cash) is unsolved *because* of the vacuum. Independent analysis (Chainalysis merchant adoption data, academic fork studies, L2 adoption metrics) supports the reading that semantic instability suppresses the cash use case.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because all parties pay ongoing costs — fork monitoring, upgrade coordination, application retooling, narrative management — without a convergence mechanism. The vacuum is not free. Suppression (0.42) is moderate: no single party can enforce their reading, but the *absence* of enforcement suppresses stable application-layer development (merchants exit, developers target higher-level protocols instead). Theater ratio (0.58) is high: 'whitepaper fidelity' rhetoric is performative — all sides deploy it while the actual governance is hashpower signaling, GitHub merge permissions, and social media consensus. Accessibility collapse (0.38) is moderate: alternatives (other chains, L2s, TradFi) exist but carry switching costs. Resistance (0.71) is high: repeated fork attempts, BIP processes, and governance proposals show active contestation.
 *
 * PERSPECTIVAL GAP:
 *   Core developers experience this as a Rope: they coordinate via rough consensus and code review, and the vacuum protects them from external capture. Miners experience it as a Snare: they must signal support for whatever upgrade preserves their revenue, with no stable rule. Application developers experience it as a Piton: they pay recurring costs for a foundation that never stabilizes. Institutional holders experience it as a Mountain: the vacuum *is* the value proposition (no governance risk = no political risk). The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (core developers, miners, institutions) hold interpretive leverage — they define what 'Bitcoin' means in practice. Their exit is arbitrage (institutions) or constrained (developers/miners locked by sunk cost). Victims (app developers, users, merchants) bear semantic instability costs with no interpretive voice. Their exit is constrained (users) or trapped (merchants who invested in Bitcoin-specific infrastructure). The vacuum benefits those with hashpower/code-control and harms those building on semantic stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (double-spending without trusted third party) was solved by the whitepaper's mechanism. The interpretive vacuum was not the solution — it was the founder's exit. The constraint persists because the vacuum prevents any single party from changing the rules, which *looks like* coordination. But the coordination is negative (preventing capture) not positive (enabling convergence). Mandatrophy is unresolved: the vacuum has outlived any founding function and now extracts via fork/upgrade costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oracle_opacity_vs_designed_absence,
    'Is the interpretive vacuum after Nakamoto''s disappearance a designed feature (intended decentralization) or an unintended structural consequence (contested substrate)?',
    'Analysis of Nakamoto''s final communications and early governance patterns; comparison with other founder-disappearance systems (e.g., Linux after Torvalds'' hypothetical exit).',
    'If designed feature, the constraint is a genuine Mountain of decentralization architecture. If unintended consequence, it is a Tangled Rope where coordination function (preventing centralized capture) and extraction (fork wars, opportunity cost of stalled upgrades) are structurally fused.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_opacity_vs_designed_absence, conceptual, 'Whether founder absence was intentional design or contingent outcome.').

omega_variable(
    coordination_extraction_boundary,
    'Does the interpretive vacuum genuinely coordinate (preventing any single party from defining ''Bitcoin'') or primarily extract (forcing all parties to pay fork/upgrade costs, suppressing applications needing stable semantics)?',
    'Measure upgrade failure rate, fork proliferation count, and application-layer abandonment rates attributable to semantic instability; compare with systems having explicit governance.',
    'If coordination dominates, the constraint trends toward Rope/Scaffold. If extraction dominates (fork costs, stranded development, merchant exit), it trends toward Snare/Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the vacuum''s primary structural function is coordination or extraction.').

omega_variable(
    whitepaper_as_kernel_ontology,
    'Is the whitepaper a genuine kernel (fixed text with determinate meaning) or a Rorschach substrate (text whose meaning is constituted by the reading community)?',
    'Linguistic/philosophical analysis of whitepaper term specificity; historical tracking of term drift (''cash'', ''peer-to-peer'', ''decentralization'') across readings.',
    'If genuine kernel, the constraint is the vacuum itself. If Rorschach substrate, the constraint is the *absence of a kernel* — a different structural object entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whitepaper_as_kernel_ontology, conceptual, 'Ontological status of the whitepaper as interpretive anchor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_oracle_opacity_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.05).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2019, 0.52).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2021, 0.56).
narrative_ontology:measurement(btc_oracle_opacity_tr_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2023, 0.58).

% Extraction over time
narrative_ontology:measurement(btc_oracle_opacity_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.15).
narrative_ontology:measurement(btc_oracle_opacity_be_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2013, 0.28).
narrative_ontology:measurement(btc_oracle_opacity_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(btc_oracle_opacity_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(btc_oracle_opacity_be_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(btc_oracle_opacity_be_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(btc_oracle_opacity_be_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(btc_oracle_opacity_su_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2011, 0.1).
narrative_ontology:measurement(btc_oracle_opacity_su_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2013, 0.22).
narrative_ontology:measurement(btc_oracle_opacity_su_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(btc_oracle_opacity_su_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2017, 0.4).
narrative_ontology:measurement(btc_oracle_opacity_su_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement(btc_oracle_opacity_su_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(btc_oracle_opacity_su_t2023, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.08).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose__store_of_value_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_block_size_governance).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_layer2_emergence).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: the whitepaper title 'Bitcoin: A Peer-to-Peer Electronic Cash System' contains two binding terms ('cash' and 'peer-to-peer') that pull in opposite directions under oracle opacity. This reading (oracle opacity) is the meta-constraint enabling the sibling readings' mutual incompatibility. The electronic_cash_reading and store_of_value_reading are downstream constraints instantiated by specific interpretive commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, institutional, 0.15).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, organized, 0.25).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, moderate, 0.7).
constraint_indexing:directionality_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
