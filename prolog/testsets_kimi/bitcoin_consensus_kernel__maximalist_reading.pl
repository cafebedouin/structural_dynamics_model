% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bitcoin Consensus Kernel â Maximalist Reading: Immutable Monetary Covenant
 *   domain: cryptoeconomic/monetary/distributed_consensus
 *
 * SUMMARY:
 *   The Bitcoin maximalist reading treats the protocol's monetary
 *   rulesâspecifically the 21 million cap and the issuance scheduleâas an
 *   immutable covenant established by the whitepaper. Any proposal to alter
 *   block size, opcodes, or consensus rules is framed as a violation of
 *   property rights and the founding text. This creates a constraint on
 *   protocol evolution that protects incumbent holder wealth but extracts
 *   optionality from innovators, payment integrators, and future users. Key
 *   agents by structural relationship: holders and early adopters are the
 *   beneficiaries (organized/powerful, identity-locked or arbitrage exit);
 *   scalability innovators, layer-2 builders, and payment integrators are the
 *   targets (moderate power, constrained exit); hashpower and node operators
 *   enforce the covenant (organized, constrained exit); monetary economists
 *   observe the structural capture (analytical).
 *
 * KEY AGENTS:
 *   - holders (beneficiary, organized, identity_locked): Fused identity with the immutable covenant; oppose all base-layer changes.
 *   - early_adopters (beneficiary, powerful, arbitrage): Wealth and influence concentrated by fixed supply; steer funding toward stasis.
 *   - scalability_innovators (payer, moderate, constrained): Propose protocol improvements rejected as covenant violations.
 *   - layer2_builders (payer, moderate, constrained): Forced to engineer around minimal L1 capabilities.
 *   - payment_integrators (payer, moderate, constrained): Bear cost of high fees and limited throughput.
 *   - consensus_enforcers (agenda_setter, organized, constrained): Enforce no-change rules via hashpower and client coordination.
 *   - monetary_economists (observer, analytical, analytical): Track the conversion of technical consensus into political covenant.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__maximalist_reading, 0.85).
domain_priors:suppression_score(bitcoin_consensus_kernel__maximalist_reading, 0.8).
domain_priors:theater_ratio(bitcoin_consensus_kernel__maximalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__maximalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__maximalist_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__maximalist_reading, "Bitcoin Consensus Kernel â Maximalist Reading: Immutable Monetary Covenant").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__maximalist_reading, "cryptoeconomic/monetary/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__maximalist_reading, '49d83ea5-3ae6-46de-aff2-729fc14d5ac7').
narrative_ontology:cs_kernel_codification('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', fixed_text).
narrative_ontology:cs_authority_grounding('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', lineage).
narrative_ontology:cs_interpretation_layer_present('49d83ea5-3ae6-46de-aff2-729fc14d5ac7').
narrative_ontology:cs_reading_relation('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', bitcoin_consensus_kernel__utility_reading, forecloses).
narrative_ontology:cs_reading_relation('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', foundational, monetary_policy_immutable).
narrative_ontology:cs_axiom_status(monetary_policy_immutable, holdable).
narrative_ontology:cs_axiom_grounding('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', monetary_policy_immutable, deontological).
narrative_ontology:cs_axiom('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', foundational, protocol_change_equals_attack).
narrative_ontology:cs_axiom_status(protocol_change_equals_attack, holdable).
narrative_ontology:cs_axiom_grounding('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', protocol_change_equals_attack, deontological).
narrative_ontology:cs_reference_frame('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', whitepaper_monetary_covenant).
narrative_ontology:cs_drift_state('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', contemporary_ossified_network, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49d83ea5-3ae6-46de-aff2-729fc14d5ac7', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, holders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__maximalist_reading, early_adopters).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, scalability_innovators).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, layer2_builders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__maximalist_reading, payment_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold bitcoin and benefit from a fixed supply schedule that prevents dilution. Their social identity is fused with the protocol's immutability; they actively oppose any base-layer change as a violation of the founding covenant and their property rights. Exit is economically possible but identity-costly.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, holders, beneficiary,
    organized, generational, identity_locked, global).

% Accumulated bitcoin at low cost and benefit disproportionately from fixed supply and network effects. They influence development funding, media narratives, and social consensus to enforce stasis. Economic exit is easy, but ideological and reputational exit within their communities is costly.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Propose base-layer changes to increase throughput or reduce fees. Their proposals are systematically rejected or framed as attacks under the maximalist reading. They bear the cost of a constrained innovation space and must either abandon Bitcoin or engineer around deliberate L1 limitations.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, scalability_innovators, payer,
    moderate, biographical, constrained, global).

% Build secondary infrastructure to route around L1 throughput limits. They depend on L1 script capabilities and block space, which the maximalist reading keeps intentionally minimal. Their engineering is more complex and capital-intensive because base-layer upgrades that would ease their work are blocked as covenant violations.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, layer2_builders, payer,
    moderate, biographical, constrained, global).

% Attempt to integrate Bitcoin for retail or everyday payments but face volatile fees and limited throughput due to the refusal to alter block size or introduce protocol-level efficiency improvements. They bear the cost of Bitcoin's reduced utility as a medium of exchange.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, payment_integrators, payer,
    moderate, biographical, constrained, global).

% Operate mining and validation infrastructure. Under the maximalist reading, they enforce protocol rules by rejecting non-conforming blocks and coordinating client software upgrades that preserve the no-change covenant. Their revenue depends on chain continuity, and they enforce the stasis agenda through hashpower signaling and social coordination.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, consensus_enforcers, agenda_setter,
    organized, biographical, constrained, global).

% Study monetary systems and distributed consensus. They observe that the maximalist reading converts a technical consensus mechanism into an unchangeable political covenant, extracting optionality from future participants while preserving incumbent wealth.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__maximalist_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a credibly fixed monetary supply schedule that no future participant can unilaterally alter, solving the coordination problem of trusting a distributed money not to inflate arbitrarily.
% TRANSFER_FUNCTION: Moves optionality and innovation capacity from future builders, payment integrators, and scalability engineers to incumbent holders and early adopters by permanently locking the base layer against protocol upgrades.
% ABSENT_VOICES: Future users who need cheap payments, developers who would improve the base layer, and monetary economists who question whether a fixed supply is optimal under all macroeconomic conditions are structurally excluded from governance; their exclusion is enforced by framing any dissent as an attack on property rights.
% DISAPPEARANCE_RATIONALE: If the maximalist reading vanished, hard-fork proposals would proliferate, base-layer scaling upgrades would be evaluated on technical rather than covenant grounds, the scarcity premium maintained by enforced stasis would compress, and Bitcoin's development culture would shift from covenant-enforcement to utilitarian protocol evolution.
% FOUNDING_PROBLEM: Fiat monetary systems suffer from arbitrary inflation, opaque monetary policy, and capture by state actors, eroding savings and distorting economic coordination; additionally, electronic payments require trusted third parties.
% FOUNDING_PROBLEM_CORROBORATION: Cypherpunk mailing list archives and the whitepaper itself attest the founding problem from outside the current holder beneficiary set. Independent monetary historians and economists outside the Bitcoin space corroborate the problem of fiat inflation and trusted-third-party risk, though they dispute whether the maximalist reading is the necessary or optimal solution.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__maximalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__maximalist_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.85) because the reading actively blocks protocol changes that would benefit innovation layers, transferring surplus to incumbent holders via enforced scarcity and optionality reduction. Suppression is high (0.80) because dissenting development paths are socially and technically ostracized as 'attacks.' Theater_ratio is moderate (0.45): the fixed supply is a real coordination mechanism, but the 'covenant' framing and performative rejection of all changes serve to obscure the asymmetric extraction from future participants. Accessibility_collapse is high (0.75) because within the maximalist framework, alternatives such as block size increases or soft forks for efficiency are framed as impossible or illegitimate. Resistance is high (0.70) because the victim groups actively propose alternatives and build competing layers, but are suppressed by the agenda-setter coalition. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The holder seat experiences the constraint as protective property-rights enforcement (directionality near the beneficiary end), while the scalability innovator seat experiences it as extractive obstruction of necessary evolution (directionality near the target end). The early adopter with arbitrage-grade exit sits closer to beneficiary than the identity-locked holder, though both are in the beneficiary group. The engine computes this divergence from the structural dataâthe authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (holders, early_adopters) receive low directionality: the constraint subsidizes their position by preventing dilution or competition from protocol evolution. Victims (scalability_innovators, layer2_builders, payment_integrators) receive high directionality: the constraint extracts from them by blocking base-layer improvements they need. The consensus_enforcers (agenda_setter) have intermediate directionalityâthey enforce but do not uniquely capture the extraction, aligning with the coordinated aspect of the tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview, this constraint could be misread as a Mountain (immutable natural law) or a Rope (pure coordination around fixed supply). The genealogy reveals it was built to solve a specific problemâtrustless electronic cash and inflation resistanceânot to enshrine a permanent covenant against all change. The persistence of the maximalist reading after the founding problem shifted (from niche experiment to global asset) suggests the coordination function has atrophied into extraction, yet the fixed supply mechanism remains a genuine coordination solution, preventing the classification from collapsing into pure Snare. This is the Tangled Rope signature: genuine coordination plus asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_vs_cover,
    'Is the maximalist insistence on immutability a genuine covenant to preserve monetary trust, or an extraction mechanism that uses covenant language to preserve incumbent wealth and suppress competing innovations?',
    'Historical analysis of upgrade proposals: if proposals with zero inflationary impact (e.g., efficiency improvements, new opcodes) were also resisted on covenant grounds, the reading leans toward extraction cover. If only inflationary or trust-altering proposals are resisted, it leans toward genuine monetary coordination.',
    'If the former, the effective extractiveness is higher than measured and the coordination function is thinner; if the latter, the extraction is defensive and narrower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_cover, conceptual, 'Whether maximalism is genuine coordination or incumbent cover').

omega_variable(
    kernel_authority_locus,
    'Does the maximalist reading''s authority derive from the whitepaper text as a fixed kernel, or from the emergent social consensus of holders and miners as an implicit kernel?',
    'Discourse analysis of maximalist argumentation: appeals to ''Satoshi''s vision'' versus appeals to ''what the market or node operators accept'' reveal the true kernel grounding.',
    'If authority is whitepaper-textual, changes are foreclosed by interpretation-layer doctrine; if authority is emergent-consensus, the reading is more volatile and susceptible to gradual drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_locus, conceptual, 'Ambiguity in the locus of authority for the maximalist reading').

omega_variable(
    sibling_reading_pressure,
    'Does the maximalist reading''s dominance structurally foreclose the utility_reading and pragmatic_synthesis, or merely coexist as one faction among many?',
    'Measure developer funding flows, grant-making criteria, and social-media discourse share: if non-maximalist developers are systematically defunded or excluded from core infrastructure, the relation is closer to foreclosure via extraction; if alternative layers thrive without L1 change, it is coexistence.',
    'If foreclosing, the network edge should be strong and the constraint leans snare; if coexisting, the tangled rope classification holds because coordination and extraction operate in parallel without fully suppressing sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_pressure, empirical, 'Whether maximalist dominance forecloses or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__maximalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_max_tr_t0, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_max_tr_t3, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(btc_max_tr_t6, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(btc_max_tr_t9, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement(btc_max_tr_t12, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(btc_max_tr_t15, bitcoin_consensus_kernel__maximalist_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(btc_max_be_t0, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(btc_max_be_t3, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(btc_max_be_t6, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(btc_max_be_t9, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(btc_max_be_t12, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(btc_max_be_t15, bitcoin_consensus_kernel__maximalist_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(btc_max_su_t0, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(btc_max_su_t3, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(btc_max_su_t6, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(btc_max_su_t9, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(btc_max_su_t12, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(btc_max_su_t15, bitcoin_consensus_kernel__maximalist_reading, suppression_requirement, 15, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__maximalist_reading, identity_coordination).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__utility_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__maximalist_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_consensus_kernel. It decomposes from the colloquial label 'Bitcoin consensus rules' into structurally distinct claims: the maximalist reading (this file, high extraction via covenant enforcement), the pragmatic synthesis (base immutable, layers innovate), and the utility reading (consensus as minimum viable enabling iterative improvement). Each reading has a different beneficiary/victim structure and different epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
