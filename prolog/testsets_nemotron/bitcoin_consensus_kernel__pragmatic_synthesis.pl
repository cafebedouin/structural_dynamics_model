% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__pragmatic_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__pragmatic_synthesis, []).

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
 *   constraint_id: bitcoin_consensus_kernel__pragmatic_synthesis
 *   human_readable: Bitcoin Base Layer Immutability with Layered Innovation
 *   domain: cryptoeconomics/monetary_systems/distributed_consensus
 *
 * SUMMARY:
 *   The pragmatic_synthesis reading of the Bitcoin consensus kernel asserts
 *   that the base layer's monetary rules (21M cap, emission schedule,
 *   difficulty adjustment) are immutable — a Mountain-like commitment — while
 *   upper layers (Lightning, Liquid, Stacks, rollups) permit open innovation
 *   without violating the kernel. This reading attempts to satisfy both the
 *   maximalist demand for monetary credibility and the utility demand for
 *   programmable money. The constraint is a scaffold: it presents the
 *   base/layer segregation as a transitional arrangement that buys time for
 *   layer2 maturity, with the implicit sunset being when layer2 volume and
 *   trust-minimization make the base layer purely a settlement anchor. The
 *   victim is ideological coherence — maximalists lose the unitary 'one
 *   chain, one rule' narrative that grounds their moral claim.
 *
 * KEY AGENTS:
 *   - base_layer_operators: Institutional agenda_setter (institutional/biographical/arbitrage/global) — miners, node operators, and custodians who enforce base layer rules and capture settlement fees
 *   - layer2_developers: Beneficiary (organized/generational/mobile/global) — builders on Lightning, Liquid, rollups who gain programmable money without consensus risk
 *   - institutional_investors: Beneficiary (institutional/biographical/arbitrage/global) — capital allocators who require credible monetary policy as a prerequisite for allocation
 *   - application_users: Beneficiary (organized/biographical/constrained/global) — end users who get low-fee, fast transactions via layers while trusting base layer settlement
 *   - maximalist_ideologues: Victim (powerful/generational/identity_locked/global) — developers, commentators, and holders for whom the kernel's unity is the value proposition
 *   - consensus_purists: Victim (moderate/generational/trapped/global) — actors who believe any delegation of trust to layers violates the kernel's trust-minimization guarantee
 *   - protocol_researchers: Observer (analytical/civilizational/analytical/universal) — analysts tracking whether the base/layer distinction holds under adversarial conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__pragmatic_synthesis, 0.12).
domain_priors:suppression_score(bitcoin_consensus_kernel__pragmatic_synthesis, 0.25).
domain_priors:theater_ratio(bitcoin_consensus_kernel__pragmatic_synthesis, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, extractiveness, 0.12).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__pragmatic_synthesis, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__pragmatic_synthesis, scaffold).
narrative_ontology:human_readable(bitcoin_consensus_kernel__pragmatic_synthesis, "Bitcoin Base Layer Immutability with Layered Innovation").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__pragmatic_synthesis, "cryptoeconomics/monetary_systems/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__pragmatic_synthesis).
narrative_ontology:has_sunset_clause(bitcoin_consensus_kernel__pragmatic_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__pragmatic_synthesis, '5abc8eb4-9669-4af0-9d70-ec218c2e9bb9').
narrative_ontology:cs_kernel_codification('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', fixed_text).
narrative_ontology:cs_authority_grounding('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', lineage).
narrative_ontology:cs_interpretation_layer_present('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9').
narrative_ontology:cs_reading_relation('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', bitcoin_consensus_kernel__maximalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', bitcoin_consensus_kernel__utility_reading, coexists_with).
narrative_ontology:cs_axiom('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', foundational, base_layer_monetary_rules_immutable).
narrative_ontology:cs_axiom_status(base_layer_monetary_rules_immutable, holdable).
narrative_ontology:cs_axiom_grounding('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', base_layer_monetary_rules_immutable, conventional).
narrative_ontology:cs_axiom('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', foundational, upper_layers_permit_innovation_without_kernel_violation).
narrative_ontology:cs_axiom_status(upper_layers_permit_innovation_without_kernel_violation, holdable).
narrative_ontology:cs_axiom_grounding('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', upper_layers_permit_innovation_without_kernel_violation, instrumental).
narrative_ontology:cs_reference_frame('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', whitepaper_consensus_mechanism).
narrative_ontology:cs_drift_state('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', post_layer2_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5abc8eb4-9669-4af0-9d70-ec218c2e9bb9', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, layer2_developers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, institutional_investors).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__pragmatic_synthesis, application_users).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, consensus_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__pragmatic_synthesis, application_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Miners, node operators, and custodians who enforce base layer consensus rules. They capture settlement fees and control protocol upgrades. Their exit is arbitrage-grade: they can redirect hash power or custody to other chains, but the network effect locks them in. They benefit from the immutable kernel as a credible commitment device.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, base_layer_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Builders on Lightning, Liquid, Stacks, rollups, and other layer2 systems. They gain programmable money, fast transactions, and complex scripting without risking base layer consensus. Their exit is mobile: they can build on competing base layers (Ethereum, Solana) but Bitcoin's liquidity and credibility create a gravitational pull.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, layer2_developers, beneficiary,
    organized, generational, mobile, global).

% Capital allocators (funds, treasuries, nation-states) who require credible monetary policy (21M cap, predictable emission) as a prerequisite for allocation. They benefit from the base/layer split: the base layer gives them the monetary credibility they need; layers give them the programmable infrastructure for financial products. Their exit is arbitrage-grade across asset classes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, institutional_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% End users who transact via layer2s for low fees and speed, while relying on base layer settlement for finality. They benefit from the coordination but pay layer2 fees and bear layer2 trust assumptions. Their exit is constrained: they can use other payment rails, but Bitcoin's monetary properties are the draw.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, application_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__pragmatic_synthesis, application_users, payer).

% Developers, commentators, and holders for whom Bitcoin's value IS its unitary, unchanging nature. The base/layer split extracts their ideological coherence: they must either accept that 'Bitcoin' now includes layers (violating their core premise) or fork/exit (abandoning the network effect). Their identity is fused to the kernel's unity — exit is identity_locked.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, maximalist_ideologues, payer,
    powerful, generational, identity_locked, global).

% Actors who believe any trust delegation to layers (federations, committees, honest-majority assumptions) violates the kernel's trust-minimization guarantee. They are trapped: no alternative chain offers Bitcoin's liquidity and credibility, but the layered system violates their participation condition. They cannot exit without losing the very property they value.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, consensus_purists, payer,
    moderate, generational, trapped, global).

% Analysts tracking whether the base/layer distinction holds under adversarial conditions (state attacks, MEV, quantum threats, governance capture). They neither collect nor pay; they measure whether the scaffold's coordination function persists or whether extraction accumulates.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__pragmatic_synthesis, protocol_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__pragmatic_synthesis, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__pragmatic_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables incompatible factions (maximalists who demand immutable monetary policy, utility-seekers who demand programmable money) to share a single kernel without either conceding their core requirement. The base layer provides credible neutrality; layers provide innovation space.
% TRANSFER_FUNCTION: Moves ideological coherence from maximalists (who lose the unitary 'one chain, one rule' narrative) to layer2 developers and institutional investors (who gain a credible, programmable monetary system). No direct monetary transfer — the extraction is semantic and structural.
% ABSENT_VOICES: Sovereign states considering Bitcoin as reserve asset but blocked by regulatory uncertainty around layered systems; unbanked populations who need simple, non-custodial payments but get complex layer2 UX; miners in jurisdictions where layer2 fee revenue cannot substitute for base layer subsidies post-halving.
% DISAPPEARANCE_RATIONALE: If the base/layer distinction vanished overnight, maximalists would likely hard-fork to preserve unitary consensus; layer2 developers would migrate to more permissive base layers (Ethereum, Solana); institutional investors would reallocate to assets with clearer governance; the Bitcoin ecosystem would fragment into competing kernels.
% FOUNDING_PROBLEM: Bitcoin's founding tension: the whitepaper presents a system that is simultaneously 'a purely peer-to-peer version of electronic cash' (requiring programmable utility) and a fixed monetary policy (requiring immutable rules). The pragmatic_synthesis reading was built to solve this tension by segregating the two functions across layers.
% FOUNDING_PROBLEM_CORROBORATION: Lightning Network developers and researchers (outside the maximalist beneficiary set) attest the founding problem is live — layer2 adoption, UX, and trust-minimization remain incomplete. Maximalist developers attest the problem is live but argue the solution violates the kernel. The coexistence of these attestations from opposed seats corroborates the 'live' status.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__pragmatic_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__pragmatic_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bitcoin_consensus_kernel__pragmatic_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__pragmatic_synthesis, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).
:- end_tests(bitcoin_consensus_kernel__pragmatic_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint primarily coordinates — it lets incompatible factions share a kernel without either conceding. The extraction that exists is the ideological coherence tax on maximalists: they must accept that 'Bitcoin' now means a layered system, not a unitary chain. Suppression (0.25) is modest — maximalists can still run nodes, mine, and advocate; they are not prevented from forking. Theater (0.18) rises over time as the base/layer distinction becomes ritualized in governance discourse (BIP processes, layer2 marketing) while the actual power to change base rules remains concentrated in a small operator set. The time grid is shared across all three metrics at 0, 4, 8, 12, 16, 20 (roughly 2016-2024 in 4-year epochs).
 *
 * PERSPECTIVAL GAP:
 *   From the operator/investor seat, this is a rope/scaffold — genuine coordination solving the credibility/innovation tradeoff. From the maximalist seat, it is a snare — the kernel's unity is being hollowed out while the label 'Bitcoin' is captured. The engine computes this divergence from the structural data; the claimed_type (scaffold) reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Base layer operators are near-beneficiary (d ~ 0.15): they control the immutable kernel and capture settlement revenue. Layer2 developers and institutional investors are beneficiaries (d ~ 0.2-0.3): they gain innovation space and credible neutrality without bearing base layer maintenance costs. Maximalist ideologues are targets (d ~ 0.85): their identity is fused to the unitary kernel narrative; the base/layer split extracts their coherence. Consensus purists are trapped targets (d ~ 0.9): they cannot exit to a purer chain without abandoning the network effect. The engine derives these from declared beneficiaries/victims + exit options + power atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (credible money + programmable money) is live — layer2 maturity is incomplete. The mandate has not atrophied. However, if layer2 adoption stalls or base layer ossification prevents necessary upgrades (e.g., quantum resistance), the scaffold could harden into a tangled_rope where the base/layer distinction becomes permanent extraction from maximalists without delivering the promised innovation. The sunset clause is the guard against this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the pragmatic_synthesis reading a distinct constraint with stable ε, or a rhetorical bridge between maximalist_reading and utility_reading?',
    'Trace whether institutional actors (exchanges, custodians, nation-states) coordinate around the base/layer distinction as an operative commitment, or whether they treat it as a diplomatic framing that collapses under stress.',
    'If the distinction is operative, ε remains low (0.12) and the constraint is a genuine scaffold. If it collapses, the constraint reveals itself as a contested interpretation of a single kernel with ε varying by reading — requiring decomposition per ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a structurally distinct constraint or a diplomatic framing').

omega_variable(
    ideological_coherence_as_victim,
    'Does extracting ideological coherence from maximalists constitute genuine extraction (victimhood) or merely the cost of participating in a pluralistic system?',
    'Measure whether maximalist developers/miners exit the ecosystem, fork the chain, or redirect resources to competing kernels when the base/layer distinction is codified in protocol governance.',
    'If maximalists fork or exit at scale, the victim declaration is structurally grounded. If they remain and build within layers, the victim status is performative — the constraint is closer to a rope with rhetorical resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_coherence_as_victim, empirical, 'Whether maximalist ideological loss registers as structural victimhood or rhetorical posture').

omega_variable(
    scaffold_sunset_trigger,
    'What concrete condition triggers the sunset clause — when is the scaffold ''done'' and the base layer either ossified or abandoned?',
    'Identify whether the scaffold declares an explicit sunset (e.g., ''when layer2 settlement volume exceeds base layer by 100x'') or whether sunset is implicit in the maximalist/utility contest resolution.',
    'An explicit sunset makes this a canonical scaffold. An implicit sunset makes it a tangled_rope masquerading as transitional — the coordination function persists but the extraction (ideological coherence) becomes permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_trigger, conceptual, 'Whether the transitional justification has a structural termination condition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__pragmatic_synthesis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 4, 0.08).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 8, 0.12).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 12, 0.15).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 16, 0.18).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_consensus_kernel__pragmatic_synthesis, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(bitc_be_t4, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 4, 0.07).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(bitc_be_t12, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(bitc_be_t16, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(bitc_be_t20, bitcoin_consensus_kernel__pragmatic_synthesis, base_extractiveness, 20, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t4, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 4, 0.15).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(bitc_su_t12, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 12, 0.23).
narrative_ontology:measurement(bitc_su_t16, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(bitc_su_t20, bitcoin_consensus_kernel__pragmatic_synthesis, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__pragmatic_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_consensus_kernel__pragmatic_synthesis, 0.08).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, lightning_network_settlement_finality).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__pragmatic_synthesis, rollup_trust_assumptions).

% DUAL FORMULATION NOTE:
% This constraint is one member of the bitcoin_consensus_kernel family. The three readings (maximalist_reading, pragmatic_synthesis, utility_reading) decompose the single colloquial label 'Bitcoin consensus rules' into structurally distinct constraints with different ε, beneficiaries, and victims. The pragmatic_synthesis reading has the lowest extractiveness (0.12) because it coordinates rather than extracts; the maximalist_reading has higher extractiveness from utility-seekers excluded from base layer changes; the utility_reading has higher extractiveness from maximalists whose veto power is overridden.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_consensus_kernel__pragmatic_synthesis, powerful, 0.85).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__pragmatic_synthesis, moderate, 0.9).
constraint_indexing:directionality_override(bitcoin_consensus_kernel__pragmatic_synthesis, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
