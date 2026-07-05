% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__protocol_ossification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: bitcoin_whitepaper__protocol_ossification_reading
 *   human_readable: Bitcoin Base-Layer Ossification: Near-Unanimous Consensus as Legitimacy Gate
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This story isolates one specific reading of the Bitcoin whitepaper's
 *   legacy: the claim that protocol changes to the base layer are
 *   illegitimate unless they approach near-universal consensus, and that
 *   stability of the settlement layer is the primary virtue to be defended.
 *   This is distinct from the p2p-cash reading (which evaluates Bitcoin as a
 *   medium of exchange and would treat throughput limits as a direct failure)
 *   and the digital-gold reading (which evaluates Bitcoin as a scarce store
 *   of value and treats the same stability as a pure feature with no named
 *   victims). Under the ossification reading, the near-unanimity norm
 *   functions as a hybrid: it genuinely coordinates trust against unilateral
 *   capture (its founding function), but over time it has also become a
 *   mechanism that structurally protects the economic position of long-term
 *   holders, layer-two businesses, and mining operators against use cases
 *   that require the base layer to change. The 2017 block-size conflict is
 *   the paradigm case: a large fraction of users and merchants wanted larger
 *   blocks for cheaper payments; a determined minority blocked it
 *   indefinitely by simply withholding consensus, and the eventual fork
 *   (Bitcoin Cash) is treated within this reading's own community as proof
 *   the norm worked, rather than as evidence of a governance failure that
 *   excluded a large constituency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__protocol_ossification_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__protocol_ossification_reading, 0.62).
domain_priors:theater_ratio(bitcoin_whitepaper__protocol_ossification_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(bitcoin_whitepaper__protocol_ossification_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__protocol_ossification_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__protocol_ossification_reading, "Bitcoin Base-Layer Ossification: Near-Unanimous Consensus as Legitimacy Gate").
narrative_ontology:topic_domain(bitcoin_whitepaper__protocol_ossification_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__protocol_ossification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__protocol_ossification_reading, 'aaaa2e17-a42a-4186-9056-7408cbd6f56d').
narrative_ontology:cs_kernel_codification('aaaa2e17-a42a-4186-9056-7408cbd6f56d', fixed_text).
narrative_ontology:cs_authority_grounding('aaaa2e17-a42a-4186-9056-7408cbd6f56d', practice).
narrative_ontology:cs_interpretation_layer_present('aaaa2e17-a42a-4186-9056-7408cbd6f56d').
narrative_ontology:cs_reading_relation('aaaa2e17-a42a-4186-9056-7408cbd6f56d', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_reading_relation('aaaa2e17-a42a-4186-9056-7408cbd6f56d', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_axiom('aaaa2e17-a42a-4186-9056-7408cbd6f56d', foundational, stability_is_primary_virtue).
narrative_ontology:cs_axiom_status(stability_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('aaaa2e17-a42a-4186-9056-7408cbd6f56d', stability_is_primary_virtue, conventional).
narrative_ontology:cs_axiom('aaaa2e17-a42a-4186-9056-7408cbd6f56d', foundational, near_unanimity_is_sole_legitimate_change_process).
narrative_ontology:cs_axiom_status(near_unanimity_is_sole_legitimate_change_process, holdable).
narrative_ontology:cs_axiom_grounding('aaaa2e17-a42a-4186-9056-7408cbd6f56d', near_unanimity_is_sole_legitimate_change_process, conventional).
narrative_ontology:cs_reference_frame('aaaa2e17-a42a-4186-9056-7408cbd6f56d', satoshi_era_rough_consensus_norm).
narrative_ontology:cs_drift_state('aaaa2e17-a42a-4186-9056-7408cbd6f56d', post_block_size_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aaaa2e17-a42a-4186-9056-7408cbd6f56d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, core_protocol_developers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__protocol_ossification_reading, layer_two_infrastructure_builders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, merchants_needing_cheap_base_layer_payments).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, developing_market_remittance_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, altcoin_style_feature_proponents).
narrative_ontology:constraint_victim(bitcoin_whitepaper__protocol_ossification_reading, users_priced_out_by_fee_spikes).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, credibly_neutral_monetary_policy).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__protocol_ossification_reading, minimal_trust_assumptions_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reference client and gatekeep which changes are even proposed for soft-fork activation. Their professional identity and reputational capital are built on stewarding conservatism; they can build indefinitely on higher layers regardless of base-layer changes, so they bear little of the cost of blocking base-layer evolution while controlling the process that blocks it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, core_protocol_developers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold bitcoin primarily as a store of value and benefit directly from predictable, unchanging monetary policy and settlement rules. A frozen protocol maximizes the credibility of the 21-million-coin promise they have staked their holdings on; any base-layer change that could alter throughput, fee markets, or supply assumptions is read by this group as an existential threat rather than an improvement.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, long_term_holders, beneficiary,
    organized, civilizational, mobile, global).

% Have invested capital in specialized hardware tuned to the current fee-and-block-reward equilibrium. Near-unanimous consensus requirements give them an effective veto over changes that might alter transaction throughput or fee dynamics; they can withhold hash power support to stall any proposal that threatens their revenue model.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__protocol_ossification_reading, mining_pool_operators, agenda_setter).

% Have built businesses (payment channels, sidechains, custodial rails) that monetize the base layer's limitations. A frozen, low-throughput base layer is the market gap their products fill; base-layer scaling changes would directly compete with their revenue.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, layer_two_infrastructure_builders, beneficiary,
    powerful, biographical, mobile, global).

% Want low, predictable transaction fees for direct on-chain settlement. Fee spikes during demand surges make base-layer bitcoin payments impractical for everyday commerce; adopting a layer-two solution requires trusting additional counterparties or technical complexity, and lobbying for a base-layer capacity increase runs into the near-unanimity requirement, which effectively never yields consensus while any faction objects.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, merchants_needing_cheap_base_layer_payments, payer,
    moderate, biographical, constrained, global).

% Rely on cross-border transfers where every dollar of fee matters. When base-layer fees spike, they are pushed either to alternative (often less secure) chains, to costly custodial intermediaries, or out of the system entirely. They have no seat in the developer mailing lists or mining pool governance where the near-unanimity threshold is actually negotiated.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, developing_market_remittance_users, payer,
    powerless, immediate, trapped, global).

% Propose features such as larger blocks, changed monetary parameters, or smart-contract expansions and are routinely characterized as attacking Bitcoin's identity rather than improving it. Their technical arguments are heard but structurally cannot clear the near-universal-consensus bar because any single influential objector can block indefinitely; most eventually fork off entirely, which is treated as proof the ossification norm worked as intended.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, altcoin_style_feature_proponents, excluded,
    moderate, biographical, constrained, global).

% During periods of high mempool congestion, ordinary users find on-chain transactions economically irrational for small transfers. They absorb this as a fact of using bitcoin, with no organized channel to push for a capacity change given the effective veto structure.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, users_priced_out_by_fee_spikes, payer,
    powerless, immediate, trapped, global).

% Study the social and technical mechanics of the near-unanimity requirement, comparing outcomes across the 2017 block-size conflict and later soft forks, without a stake in which faction wins.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__protocol_ossification_reading, protocol_change_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A near-universal consensus requirement genuinely coordinates trust: it prevents any single faction — including the core developers themselves — from unilaterally rewriting monetary policy or settlement rules, which is the credibility property that makes the asset's scarcity claim durable.
% TRANSFER_FUNCTION: The arrangement moves the cost of protocol conservatism onto use cases that require base-layer change (cheap payments, high throughput, new feature sets) and transfers a durability/scarcity premium to holders and businesses whose models depend on the base layer staying exactly as it is.
% ABSENT_VOICES: Proponents of base-layer capacity increases and alternative monetary parameters are structurally present in mailing lists and forums but functionally excluded from the outcome, since the consensus bar lets any determined minority veto change indefinitely; users in low-fee-tolerant markets have essentially no representation in the informal governance process at all.
% DISAPPEARANCE_RATIONALE: If the near-unanimity norm vanished overnight, long-term holders and layer-two builders would argue the world rearranges catastrophically (monetary credibility destroyed, hard-fork chaos, hash power splits); merchants and remittance users would argue the world mostly stays the same for them except that a base-layer capacity increase becomes newly possible, improving their situation; the parties fundamentally disagree on which counterfactual is the real one, which is itself evidence the norm functions as an identity commitment as much as an engineering choice.
% FOUNDING_PROBLEM: Early Bitcoin had no established governance process at all; without some norm for evaluating proposed changes, either developers could push through experimental changes with untested consequences (as nearly happened with the 2010 value-overflow bug and later hard-fork attempts), or a well-resourced minority could rewrite the protocol's core promises out from under long-term holders.
% FOUNDING_PROBLEM_CORROBORATION: Long-term holders and core developers attest the founding problem (need for credible commitment against unilateral change) remains fully live, citing the 2017 block-size war as proof a determined faction will try to rewrite the rules absent the norm. Independent academics studying blockchain governance and several early Bitcoin contributors who later left the project attest the norm has drifted from 'prevent unilateral capture' to 'prevent ANY base-layer evolution regardless of merit,' arguing the founding problem was narrower than the current practice now enforces.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__protocol_ossification_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__protocol_ossification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__protocol_ossification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__protocol_ossification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__protocol_ossification_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts moderate (0.30) reflecting the genuine coordination value of the norm in Bitcoin's early years, when few competing economic interests had crystallized around base-layer stasis. It rises steadily to 0.58 by the interval's end as the population of stakeholders whose businesses depend specifically on the base layer NOT changing (layer-two infrastructure, mining hardware tuned to current parameters) grew, and as the near-unanimity bar increasingly protected their position rather than merely preventing capture. Theater ratio climbs in parallel (0.18 to 0.44): a growing share of the 'preserving decentralization' rhetoric functions as post-hoc justification for blocking changes that would harm specific commercial interests, while the genuine decentralization-preservation function persists but is a shrinking fraction of the total activity. Suppression rises sharply through the 2017 conflict period (time points 3-6) reflecting active mobilization to block the block-size increase, then plateaus at a high level as the norm's enforcement becomes normalized rather than actively contested.
 *
 * PERSPECTIVAL GAP:
 *   From the core-developer and long-term-holder seat, this looks like principled conservatism protecting a scarce, credibly-neutral asset — closer to a rope or even a mountain-adjacent natural constraint of decentralized coordination. From the merchant and remittance-user seat, the same near-unanimity requirement looks like an entrenched veto machine that protects incumbent economic interests behind a decentralization narrative — closer to a snare. The tangled_rope classification is claimed here because both structural elements are genuinely present: real coordination function (preventing unilateral capture) AND asymmetric extraction (protecting specific commercial interests against a specific victim class), with active enforcement (social ostracism of block-size proponents, refusal of pull requests, hash-power signaling) required to sustain it.
 *
 * DIRECTIONALITY LOGIC:
 *   Core developers, long-term holders, mining operators, and layer-two builders sit near the beneficiary end: the constraint either directly funds their business model or protects the specific value proposition they have bet on. Merchants, remittance users, and fee-sensitive users sit near the target end: they bear the cost of a base layer that cannot scale to meet their use case, with no meaningful path to change it given the consensus threshold. Note the asymmetry in exit options — long-term holders and layer-two builders have `mobile` or `arbitrage` exit (they can hold other assets or pivot business models), while remittance users in constrained markets are `trapped` — they need cheap cross-border transfer and have few alternatives with comparable security guarantees.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing capture by any single faction — has arguably been solved: Bitcoin has now survived multiple attempted unilateral changes and forks without losing its core value proposition. But the norm has not sunset; it has instead expanded scope from 'prevent capture' to 'prevent essentially all base-layer evolution,' which is a different and much stronger claim than the founding problem required. This is a canonical mandatrophy candidate: the mandate (radical conservatism as capture-prevention) persists and has hardened even as its narrower founding justification has been substantially achieved, while its cost (foreclosing legitimate scaling proposals) has grown as the user base has diversified beyond the original store-of-value cohort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_prevention_vs_incumbent_protection,
    'Is the near-universal-consensus requirement still functioning primarily as capture prevention (its founding purpose), or has it been substantially captured itself by parties whose commercial interests benefit from base-layer stasis?',
    'Compare the substantive content of blocked proposals against the stated capture-prevention rationale: if blocked proposals disproportionately correlate with harm to mining-hardware economics or layer-two revenue models rather than genuine security or decentralization risk, the norm has drifted into incumbent protection.',
    'If ossification is genuine capture prevention, the tangled_rope classification undercounts its coordination value and it sits closer to a scaffold that never sunset. If it is substantially captured, the classification is conservative and the constraint is closer to a snare wearing decentralization rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_prevention_vs_incumbent_protection, empirical, 'Whether the ossification norm still serves its founding anti-capture function or has become incumbent protection.').

omega_variable(
    sibling_reading_incommensurability,
    'Are the three readings of the Bitcoin whitepaper (digital gold, p2p cash, protocol ossification) genuinely independent constraints with different victim sets, or does adopting one reading necessarily commit a party to specific positions on the others?',
    'Track whether any prominent figure or institution holds the digital_gold_reading while simultaneously advocating strongly against this ossification reading (i.e., wants stability of monetary policy but supports base-layer scaling); if such positions are common and stable, the readings are genuinely separable. If they are rare, the readings may be more entangled than the ε-invariance framework treats them as being.',
    'If the readings are more entangled than modeled, the network edges between this story and its siblings should carry stronger `influences` weight; if fully separable, the current coexists_with framing in cs_structure is sufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_incommensurability, conceptual, 'Whether the three kernel readings are structurally independent or covertly coupled.').

omega_variable(
    consensus_threshold_measurement,
    'What precisely counts as ''approaching universal consensus'' in practice, and who has standing to withhold it?',
    'Historical case analysis of which actors'' objections were treated as consensus-blocking (core developers, large mining pools, exchange operators) versus which actors'' objections were disregarded (individual users, smaller merchants) across multiple proposed BIPs.',
    'If the practical threshold is really ''consensus among a small set of powerful actors'' rather than genuine universality, the suppression and extractiveness metrics may understate the concentration of effective veto power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_threshold_measurement, empirical, 'Who in practice holds veto power under the near-unanimity norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__protocol_ossification_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(bitc_tr_t16, bitcoin_whitepaper__protocol_ossification_reading, theater_ratio, 16, 0.44).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 9, 0.53).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(bitc_be_t16, bitcoin_whitepaper__protocol_ossification_reading, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(bitc_su_t16, bitcoin_whitepaper__protocol_ossification_reading, suppression_requirement, 16, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__protocol_ossification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__protocol_ossification_reading, bitcoin_whitepaper__p2p_cash_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Bitcoin whitepaper' / 'what Bitcoin is for,' per the ε-invariance principle. The digital_gold_reading treats base-layer stability as an unambiguous virtue (likely Mountain or Rope, low extraction, no victims). The p2p_cash_reading treats throughput and fee limits as a direct functional failure against the whitepaper's own stated purpose (likely Snare or Tangled Rope, victim set centered on payment use cases). This ossification_reading isolates the GOVERNANCE NORM for legitimating protocol change itself, independent of which use case Bitcoin is supposed to serve, and finds a Tangled Rope: real anti-capture coordination function, but growing asymmetric extraction protecting specific commercial incumbents against use cases requiring base-layer evolution. The three stories share no single ε because they are not the same constraint measured differently — they are three claims that happen to share a text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
