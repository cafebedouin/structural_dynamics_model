% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__protocol_ossification_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Protocol Ossification Norm
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   The Bitcoin protocol ossification reading treats the whitepaper and
 *   original protocol rules as a fixed kernel whose stability is the highest
 *   virtue. Under this reading, any base-layer change is illegitimate unless
 *   it approaches universal consensusâa threshold so high in practice that
 *   it enforces near-total immutability. The norm is actively maintained by
 *   Core maintainers, maximalist discourse, and node operator signaling. It
 *   coordinates against governance capture and monetary rule uncertainty but
 *   asymmetrically extracts from researchers, payment users, and use cases
 *   requiring on-chain evolution by externalizing innovation costs to higher
 *   layers or alternative chains. This constraint is claimed as tangled_rope
 *   because it possesses both a genuine coordination function (preventing
 *   arbitrary changes) and identifiable asymmetric extraction (protecting
 *   incumbent holders and L2 operators while blocking base-layer upgrades).
 *
 * KEY AGENTS:
 *   - bitcoin_core_maintainers: Primary agenda_setter (institutional/constrained) â administers the universal-consensus threshold through BIP process and merge decisions.
 *   - existing_bitcoin_holders: Primary beneficiary (organized/mobile) â captures rule-predictability and scarcity protection.
 *   - layer_two_entrepreneurs: Secondary beneficiary (moderate/constrained) â captures volume forced off L1.
 *   - institutional_custodians: Secondary beneficiary (powerful/constrained) â captures compliance simplicity and fee-based custody products.
 *   - protocol_researchers: Primary payer (moderate/constrained) â bears blocked-innovation cost.
 *   - retail_payment_users: Secondary payer (powerless/constrained) â bears high-fee and complexity cost.
 *   - altcoin_ecosystems: Excluded (organized/mobile) â structurally barred from the governance conversation.
 *   - monetary_economists: Analytical observer (analytical/analytical) â observes monetary credibility vs. incumbent capture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification Norm").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'c0166ca5-0982-484d-982e-52b83db572bb').
narrative_ontology:cs_kernel_codification('c0166ca5-0982-484d-982e-52b83db572bb', fixed_text).
narrative_ontology:cs_authority_grounding('c0166ca5-0982-484d-982e-52b83db572bb', lineage).
narrative_ontology:cs_interpretation_layer_present('c0166ca5-0982-484d-982e-52b83db572bb').
narrative_ontology:cs_reading_relation('c0166ca5-0982-484d-982e-52b83db572bb', bitcoin_whitepaper__digital_gold_reading, influences).
narrative_ontology:cs_reading_relation('c0166ca5-0982-484d-982e-52b83db572bb', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_axiom('c0166ca5-0982-484d-982e-52b83db572bb', foundational, protocol_stability_as_primary_virtue).
narrative_ontology:cs_axiom_status(protocol_stability_as_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('c0166ca5-0982-484d-982e-52b83db572bb', protocol_stability_as_primary_virtue, instrumental).
narrative_ontology:cs_axiom('c0166ca5-0982-484d-982e-52b83db572bb', foundational, universal_consensus_requirement).
narrative_ontology:cs_axiom_status(universal_consensus_requirement, holdable).
narrative_ontology:cs_axiom_grounding('c0166ca5-0982-484d-982e-52b83db572bb', universal_consensus_requirement, conventional).
narrative_ontology:cs_reference_frame('c0166ca5-0982-484d-982e-52b83db572bb', immutable_base_layer_protocol).
narrative_ontology:cs_drift_state('c0166ca5-0982-484d-982e-52b83db572bb', post_blocksize_contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c0166ca5-0982-484d-982e-52b83db572bb', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, existing_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer_two_entrepreneurs).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, retail_payment_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the reference implementation and BIP process. They merge or reject proposed protocol changes. Under the ossification norm, they treat even technically sound upgrades as requiring near-universal agreement that is effectively unattainable, thereby enforcing stability through procedural inertia and social pressure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_core_maintainers, agenda_setter,
    institutional, generational, constrained, global).

% Hold bitcoin as a store of value. They benefit from a static monetary policy and fixed rules that prevent dilution or unexpected changes to the asset's properties. Their wealth is protected by ossification but they do not administer the protocol.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, existing_bitcoin_holders, beneficiary,
    organized, generational, mobile, global).

% Build businesses on Lightning Network and other L2 solutions. They benefit from base-layer ossification because it drives transaction volume and necessity to their products while preventing competing L1 upgrades that might obsolete their infrastructure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer_two_entrepreneurs, beneficiary,
    moderate, biographical, constrained, global).

% Offer ETFs, custody, and treasury services. They benefit from a predictable, unchanging base layer that simplifies compliance, auditing, and risk management; frequent protocol changes would introduce operational and legal uncertainty.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, institutional_custodians, beneficiary,
    powerful, generational, constrained, global).

% Develop protocol upgrades for privacy, scaling, or functionality such as covenants, drivechains, and new signature schemes. Their proposals are routinely rejected or stalled by the universal-consensus threshold, forcing them to abandon research or migrate to other blockchains.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_researchers, payer,
    moderate, biographical, constrained, global).

% Seek to use Bitcoin for everyday transactions. They bear the cost of ossification through high on-chain fees and complexity, as base-layer throughput improvements are blocked and they are pushed toward custodial L2 solutions or alternative cryptocurrencies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, retail_payment_users, payer,
    powerless, immediate, constrained, global).

% Offer alternative base layers with more flexible upgrade paths. They are structurally excluded from the Bitcoin governance conversation and dismissed as scams or distractions, ensuring that exit from the ossification norm is delegitimized within the community.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, altcoin_ecosystems, excluded,
    organized, biographical, mobile, global).

% Study Bitcoin as a monetary experiment. They observe whether ossification produces credible neutrality or merely entrenches a particular distribution of wealth and technical control, without direct stake in the protocol's direction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(bitcoin_whitepaper__protocol_ossification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary or capture-driven changes to a monetary protocol by requiring changes to meet an extremely high bar of agreement, thereby preserving predictability and resisting governance capture by any single interest.
% TRANSFER_FUNCTION: Transfers the cost of blocked innovation and higher transaction fees to users and researchers who require base-layer changes, while transferring the benefit of rule-predictability and L2 opportunity to existing holders and layered infrastructure builders.
% ABSENT_VOICES: Alt-chain developers and base-layer scaling advocates who were driven out during the blocksize wars or silenced as altcoiners; retail users in the Global South who need low-fee on-chain payments but lack representation in the English-language developer and holder discourse.
% DISAPPEARANCE_RATIONALE: If the universal-consensus ossification norm vanished, protocol upgrades like block size increases, privacy enhancements, and new scripting capabilities would be debated on technical rather than procedural grounds; the L2 ecosystem would face competition from L1 improvements; existing holders would face uncertainty about monetary rule changes; the social and technical landscape of Bitcoin would reorganize around a more upgrade-permissive governance culture.
% FOUNDING_PROBLEM: Bitcoin's early development showed that an open-source monetary protocol with no formal governance could be changed by a small group of developers, creating the risk of capture, unexpected inflation, or contentious splits that would destroy confidence in neutral digital money.
% FOUNDING_PROBLEM_CORROBORATION: Ossification advocates cite the blocksize war as proof that governance without extreme consensus leads to civil war. Critics, including former core developers and academic governance researchers, attest that the founding problem has been replaced by a new one: minority veto and maintainer capture disguised as decentralization. No neutral third party attests the founding problem remains live in its original form; the corroboration is split along beneficiary lines.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__protocol_ossification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the universal-consensus threshold blocks upgrades that would benefit payers, effectively protecting incumbent asset positions and L2 business models at payer expense. Suppression (0.58) reflects active social enforcement: dissenting developers are ostracized, alternative clients are attacked, and contentious upgrades are procedurally buried. Theater_ratio (0.45) captures the performative aspect of universal consensus rhetoric, which often masks de facto maintainer and holder veto power. Accessibility_collapse (0.48) is moderate: empirical exits to altcoins exist but are delegitimized within the community. Resistance (0.55) is moderate-to-high, reflecting ongoing technical proposals and the lingering memory of the blocksize war. The temporal series show extraction and theater rising as the norm hardened after 2017.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (Core maintainers, holders, L2 operators) experience the constraint as necessary stewardship of neutral money; their computed type will emphasize coordination. The payer seats (researchers, retail payment users) experience the same structure as a veto that protects incumbents; their computed type will emphasize extraction. The engine derives this divergence from identical structural data because directionality inverts effective extraction for beneficiaries and amplifies it for trapped or constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing holders, L2 entrepreneurs, and institutional custodians are declared beneficiaries because the constraint's stability subsidizes their asset values and business models; their exit options and beneficiary status place their directionality near the subsidy end. Core maintainers are agenda-setters with constrained exit; their directionality is low but slightly elevated by institutional identity-lock. Protocol researchers and retail payment users are declared victims because the constraint blocks their desired use cases and forces costs onto them; their constrained or powerless position with limited exit places their directionality near the full-target end. Altcoin ecosystems are excluded rather than coordinated; the constraint's suppression mechanism is partly directed at delegitimizing them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing capture of an open-source monetary protocolâwas live during Bitcoin's early years. The classification as tangled_rope prevents mislabeling the ossification norm as pure coordination (rope) by insisting on the victim set of blocked innovators, and prevents mislabeling it as pure extraction (snare) by acknowledging the genuine capture-prevention function. If the founding problem were dead and the norm persisted purely by inertia with no live coordination function, the constraint would degrade toward piton; the measurements show theater_ratio below 0.5 and active suppression above 0.5, indicating the coordination function is still contested and enforced, not merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_consensus_as_veto,
    'Does the universal consensus requirement represent a genuine procedural safeguard against capture, or has it become a minority veto wielded by maintainers and vocal holders to block any change they dislike?',
    'Comparative analysis of upgrade success rates across blockchain governance models; measurement of whether proposed changes with broad technical support still fail due to social resistance from a small coalition.',
    'If it functions as a veto, the coordination story is cover for extraction and effective extraction is higher than measured; if genuine, the coordination function remains robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_consensus_as_veto, conceptual, 'Whether universal consensus is a safeguard or a veto mechanism').

omega_variable(
    l2_substitution_genuineness,
    'Does base-layer ossification genuinely drive valuable innovation to L2, or does it primarily force users into custodial and more complex systems that replicate traditional finance intermediation?',
    'Empirical measurement of L2 custody ratios, self-custody difficulty, and fee savings relative to potential base-layer capacity increases.',
    'If L2 substitution is mostly custodial, the constraint extracts user sovereignty and the victim set is larger; if non-custodial and efficient, the coordination benefit is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(l2_substitution_genuineness, empirical, 'Whether L2 innovation substitutes or degrades user autonomy').

omega_variable(
    kernel_reading_underdetermination,
    'Is the Bitcoin whitepaper kernel best read as mandating ossification, or does the protocol_ossification reading project a later interpretive tradition onto the original text?',
    'Historical analysis of Satoshi''s own upgrade practices and early development culture; textual analysis of the whitepaper against the ossification norm.',
    'If the reading is a later projection, the authority_grounding shifts from lineage to extraction, and the constraint''s classification would lean more heavily toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether ossification is a genuine kernel reading or a retroactive interpretive tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 9, 0.4).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 3, 0.2).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_whitepaper kernel, decomposed per the epsilon-invariance principle because the label 'Bitcoin' conflates structurally distinct claims (p2p cash, digital gold, protocol ossification governance). Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
