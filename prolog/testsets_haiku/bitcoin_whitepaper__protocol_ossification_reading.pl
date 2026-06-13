% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Protocol Ossification via Consensus Requirement
 *   domain: cryptocurrency_economics/technology_governance
 *
 * SUMMARY:
 *   Bitcoin's founding whitepaper asserts extreme protocol conservatism:
 *   changes are illegitimate unless approaching universal consensus, and
 *   stability is the primary virtue. This story instantiates ONE READING of
 *   the contested Bitcoin kernel—the protocol_ossification_reading. This
 *   reading emphasizes protocol immutability and treats the consensus
 *   requirement as the legitimate boundary condition for Bitcoin's social
 *   contract. Sibling readings—the p2p_cash_reading (emphasizing
 *   transactional capability and lower fees) and the digital_gold_reading
 *   (emphasizing store-of-value and scarcity)—focus on different aspects of
 *   the kernel and produce different victim sets. This reading is
 *   structurally distinct from its siblings: ossification sacrifices
 *   transactional expressiveness and use-case adaptability to preserve the
 *   asset's perceived stability and immutability. The constraint is CLAIMED
 *   as tangled_rope because it solves a genuine coordination problem
 *   (preventing unilateral protocol forks) while asymmetrically benefiting
 *   holders and existing interpreters at the cost of innovation and use-case
 *   expansion. The measurement series show extractiveness rising sharply in
 *   years 2-3 (the 2015-2017 period of block size and protocol expressiveness
 *   debates) as the constraint's asymmetric burden became visible, then
 *   plateauing as the constraint stabilized and community expectations
 *   adjusted.
 *
 * KEY AGENTS:
 *   - hodlers_and_long_term_holders: beneficiaries of stability, immutability preference, no enforcement cost
 *   - mining_pool_operators: beneficiaries of hardware durability, enforcement via hashpower loyalty
 *   - existing_protocol_interpreters: agenda_setters, grounding authority in Satoshi's stated intent and protocol history, identity-locked to the constraint
 *   - use_cases_requiring_protocol_changes: victims bearing layer-2 complexity costs, constrained exit
 *   - lightning_network_developers: dual-positioned, benefit from stable base but pay in constrained design space
 *   - prospective_innovations: powerless, structurally foreclosed by the constraint
 *   - alternative_blockchain_developers: excluded competitors who benefit indirectly from ossification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.72).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Protocol Ossification via Consensus Requirement").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, '2c1f4488-1769-43f3-8f03-6f99e99046fe').
narrative_ontology:cs_kernel_codification('2c1f4488-1769-43f3-8f03-6f99e99046fe', fixed_text).
narrative_ontology:cs_authority_grounding('2c1f4488-1769-43f3-8f03-6f99e99046fe', lineage).
narrative_ontology:cs_interpretation_layer_present('2c1f4488-1769-43f3-8f03-6f99e99046fe').
narrative_ontology:cs_reading_relation('2c1f4488-1769-43f3-8f03-6f99e99046fe', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c1f4488-1769-43f3-8f03-6f99e99046fe', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('2c1f4488-1769-43f3-8f03-6f99e99046fe', foundational, protocol_immutability_virtue).
narrative_ontology:cs_axiom_status(protocol_immutability_virtue, holdable).
narrative_ontology:cs_axiom_grounding('2c1f4488-1769-43f3-8f03-6f99e99046fe', protocol_immutability_virtue, deontological).
narrative_ontology:cs_axiom('2c1f4488-1769-43f3-8f03-6f99e99046fe', foundational, consensus_legitimacy_requirement).
narrative_ontology:cs_axiom_status(consensus_legitimacy_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2c1f4488-1769-43f3-8f03-6f99e99046fe', consensus_legitimacy_requirement, conventional).
narrative_ontology:cs_reference_frame('2c1f4488-1769-43f3-8f03-6f99e99046fe', satoshi_original_conservatism).
narrative_ontology:cs_drift_state('2c1f4488-1769-43f3-8f03-6f99e99046fe', contemporary_protocol_paralysis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c1f4488-1769-43f3-8f03-6f99e99046fe', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, hodlers_and_long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, existing_protocol_interpreters).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, use_cases_requiring_protocol_changes).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, lightning_network_developers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, prospective_innovations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).

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
 *   Extractiveness rises from 0.38 (early years when the constraint was informal and poorly tested) to 0.68 (current state where the consensus requirement is actively enforced and known to block improvements). The trajectory reflects the growing realization that the constraint blocks not just frivolous changes but legitimate technical progress. Theater rises from 0.12 to 0.41, indicating that as the core innovation-blocking function became visible, more of the enforcement activity is spent justifying the constraint (appeals to Satoshi's intent, immutability narratives, recursive claims about Bitcoin's virtue) rather than directly coordinating on shared rules. Suppression rises from 0.52 to 0.72, reflecting increasing enforcement intensity: developers who propose protocol changes now face organized opposition, social pressure, and fork-risk management. The measurement series are authored on a single shared time grid; every metric has a value at every measured time point.
 *
 * PERSPECTIVAL GAP:
 *   From the hodler's perspective, this is a working coordination solution—a way to prevent the protocol from being rewritten into something unrecognizable. From the innovation-blocked use-case perspective, it is a constraint that extracts the right to improve the system. From the mining pool operator's perspective, it is a guarantee that their capital investments remain durable. From the prospective-innovation perspective, it is a structural wall. The engine computes these divergent types from the stakeholder structural positions; the authored claim does not pre-adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Hodlers and mining operators: beneficiaries (low d, toward 0.0–0.2). They collect durability and wealth preservation without running the constraint. Use-case victims: high d (toward 0.8–1.0). They need protocol changes, pay in layer-2 complexity, have constrained exit options (leaving Bitcoin means losing network effects). Prospective innovations: highest d because they are trapped and identity-locked—they do not exist to represent themselves. Lightning developers sit near 0.5–0.6 because they benefit from stability but are partially victimized by inflexibility. The protocol interpreters sit near 0.3–0.4: they enforce the constraint, but their authority is grounded in claims about legitimacy that would dissolve if they unilaterally changed the rules—they are partially captured by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preventing unilateral rewrites and fork fragmentation—was genuine and acute during 2010-2013 when alternative coins abounded and Satoshi's intentions were contested. By 2018-2020, the founding problem had substantially resolved: Bitcoin's network effects locked in a single canonical version; altcoins were clearly distinct systems; Satoshi's intent was well-documented. The constraint's persistence despite founding-problem resolution is the core mandatrophy signal. The constraint is now maintained primarily by beneficiary interests (hodlers, mining pools, interpreters) and by sunk-cost identity-lock (core developers invested in the constraint's legitimacy narrative). A constraint that was built to solve fork fragmentation now prevents adoption of improvements that would consolidate Bitcoin's utility role. This is classic mandatrophy: the legitimacy story (prevent chaos) has outlived the actual chaos; the constraint is now maintained as a form of wealth protection and interpretive authority defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of uncontrolled fork fragmentation and chaotic rewrites been substantially solved, making the consensus requirement functionally obsolete even if politically maintained?',
    'Historical analysis of the actual fork landscape post-2015 combined with structured surveys of protocol-change proposals to measure the ratio of blocked improvements to prevented chaos. If blocked improvements substantially exceed prevented chaos by number and impact, founding problem is empirically resolved.',
    'If resolved, the constraint is a mandatrophy victim rather than a legitimate coordination mechanism—the maintenance story (preventing chaos) explains the constraint''s origin but not its current operation. This would shift the computed type from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding coordination problem persists or has been solved.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the protocol interpreters'' enforcement of the consensus requirement driven by identity fusion (career, reputation, belief in Satoshi''s intent) versus institutional incentive alignment (earning through fees, controlling the interpretive narrative)?',
    'Qualitative analysis of interpreter positions in debates; tracking of interpreters who have switched sides (support for higher block sizes, taproot-like changes); measurement of the personal/ideological investment statements versus explicit revenue calculations.',
    'High identity-lock would indicate the constraint is sustained by internalized suppression (interpreters cannot credibly reverse course without destroying their professional identity); low identity-lock would indicate it is purely institutional (changing the constraint would require realigning beneficiary interests). Identity-lock increases the constraint''s stability but also signals potential brittleness (identity-fused commitments can shatter suddenly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether enforcement is driven by identity fusion or institutional alignment.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the protocol_ossification_reading and the p2p_cash_reading logically foreclose each other in a single institutional framework, or do they coexist as different priorities held by different actors?',
    'Examine whether any major Bitcoin institution has coherently endorsed both rapid protocol evolution for payment use cases AND maximal immutability for asset properties, or whether the two readings are held by mutually exclusive factional coalitions. Test whether a framework could be constructed that makes both legible as compatible priorities (impossible = foreclosure; possible = coexistence).',
    'Foreclosure would indicate the kernel is genuinely split into incompatible readings; coexistence would indicate factions are simply prioritizing different aspects of a complex system. Coexistence suggests ossification is maintained by coalition politics, not logical necessity; foreclosure would suggest deeper structural incompatibility in the Bitcoin vision itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether sibling readings are logically incompatible or factionally different.').

omega_variable(
    layer_2_as_workaround_vs_incomplete,
    'Can all meaningful use cases that require protocol changes be adequately served by layer-2 and higher-layer solutions (Lightning, sidechains, rollups), or are there fundamental use-case classes that cannot be solved without protocol evolution?',
    'Systematic cataloging of proposed protocol changes and analysis of whether each can be approximated via layer-2 (with cost/complexity trade-offs) or requires base-layer support. If >80% of proposed changes have layer-2 analogues, workaround covers most needs; if <60%, there are hard limits to the workaround strategy.',
    'If layer-2 covers the space, the constraint''s victim set is smaller (complexity costs but functions achieved); if there are hard limits, the constraint forecloses certain use-case classes entirely. This reshapes the extraction assessment: extraction includes not just layer-2 complexity costs but wholesale foreclosure of some Bitcoin use cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_2_as_workaround_vs_incomplete, empirical, 'Whether protocol changes are truly necessary or approximate layer-2 workarounds exist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bitc_tr_t5, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(bitc_tr_t25, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(bitc_tr_t30, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(bitc_tr_t40, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bitc_be_t5, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(bitc_be_t25, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(bitc_be_t30, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(bitc_be_t40, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(bitc_su_t5, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(bitc_su_t25, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(bitc_su_t30, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(bitc_su_t40, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__protocol_ossification_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel is instantiated as three separate constraint stories, each representing a different reading of Satoshi's founding design. The protocol_ossification_reading prioritizes immutability and consensus legitimacy; the p2p_cash_reading prioritizes transactional capability and fee efficiency; the digital_gold_reading prioritizes scarcity and store-of-value properties. These readings coexist as different institutional factions' interpretations of the same protocol. The ossification reading influences both sibling readings by constraining the solution space—cash improvements must route through layer-2, gold arguments must justify immutability benefits. Neither sibling reading forecloses ossification within a single party's framework; the readings coexist as different priorities across different actor coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__protocol_ossification_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
