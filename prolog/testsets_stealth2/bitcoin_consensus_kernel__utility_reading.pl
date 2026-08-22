% ============================================================================
% CONSTRAINT STORY: bitcoin_consensus_kernel__utility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_consensus_kernel__utility_reading, []).

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
 *   constraint_id: bitcoin_consensus_kernel__utility_reading
 *   human_readable: Whitepaper-as-Minimum-Viable-Baseline Norm (Utility Reading)
 *   domain: cryptoeconomics/distributed_consensus
 *
 * SUMMARY:
 *   Under the utility reading, the operative norm is that the whitepaper
 *   functions as a floor specification: a minimum viable consensus mechanism
 *   that legitimates iterative improvement through backward-compatible soft
 *   forks and layered extension. The standing arrangement under contest — the
 *   referent for epsilon — is that norm as it actually operates in
 *   protocol-change practice, assessed by this reading's own lights, never
 *   the rival readings' endorsed arrangements. The arrangement coordinates
 *   split-free evolution for a leaderless network while transferring
 *   rule-certainty away from long-horizon holders and verification burden
 *   onto node operators; the development commons collects the arrangement's
 *   authority and funding gains. This file is ONE reading of the
 *   bitcoin_consensus_kernel; the maximalist and pragmatic-synthesis readings
 *   are separate constraints in separate files, linked through the network
 *   section, and the committer contest is routed to omega variables rather
 *   than folded into this classification.
 *
 * KEY AGENTS:
 *   - - core_protocol_maintainers: Agenda-setting seat (institutional/identity_locked) — administers the reference client and the change process; the arrangement's authority gains pool here and in the contributor seat below
 *   - - protocol_contributors: Primary beneficiary seat (organized/mobile) — the development commons whose work, funding, and standing the continuing-improvement mandate sustains; the seat the gains accrue to
 *   - - layer_two_builders: Beneficiary (moderate/mobile) — builds on the legitimating floor
 *   - - feature_seeking_adopters: Beneficiary (moderate/mobile) — adopts for prospective capability
 *   - - immutability_reliant_holders: Primary target (moderate/identity_locked) — bears erosion of the frozen-policy guarantee they priced in
 *   - - full_node_operators: Dual-positioned payer/beneficiary (organized/constrained) — bears the upgrade-and-verification burden, gains validation capability
 *   - - miners: Dual-positioned payer/beneficiary (organized/arbitrage) — bears activation risk and cost, gains network growth
 *   - - maximalist_faction: Excluded-from-decision voice (organized/identity_locked) — objects publicly, holds no activation seat
 *   - - cryptoeconomic_analysts: Analytical observer — maps the structure without bearing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, 0.48).
domain_priors:suppression_score(bitcoin_consensus_kernel__utility_reading, 0.35).
domain_priors:theater_ratio(bitcoin_consensus_kernel__utility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(bitcoin_consensus_kernel__utility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_consensus_kernel__utility_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_consensus_kernel__utility_reading, "Whitepaper-as-Minimum-Viable-Baseline Norm (Utility Reading)").
narrative_ontology:topic_domain(bitcoin_consensus_kernel__utility_reading, "cryptoeconomics/distributed_consensus").

domain_priors:requires_active_enforcement(bitcoin_consensus_kernel__utility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_consensus_kernel__utility_reading, '8eeb6f06-3d0d-4a34-b342-088ce0df80c7').
narrative_ontology:cs_kernel_codification('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', fixed_text).
narrative_ontology:cs_authority_grounding('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', practice).
narrative_ontology:cs_interpretation_layer_present('8eeb6f06-3d0d-4a34-b342-088ce0df80c7').
narrative_ontology:cs_reading_relation('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', bitcoin_consensus_kernel__maximalist_reading, forecloses).
narrative_ontology:cs_reading_relation('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', bitcoin_consensus_kernel__pragmatic_synthesis, coexists_with).
narrative_ontology:cs_axiom('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', foundational, whitepaper_is_iterative_design_baseline).
narrative_ontology:cs_axiom_status(whitepaper_is_iterative_design_baseline, holdable).
narrative_ontology:cs_axiom_grounding('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', whitepaper_is_iterative_design_baseline, instrumental).
narrative_ontology:cs_axiom('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', secondary, backward_compatible_evolution_preserves_kernel).
narrative_ontology:cs_axiom_status(backward_compatible_evolution_preserves_kernel, holdable).
narrative_ontology:cs_axiom_grounding('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', backward_compatible_evolution_preserves_kernel, conventional).
narrative_ontology:cs_reference_frame('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', minimum_viable_baseline_specification).
narrative_ontology:cs_drift_state('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', contemporary_multilayer_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('8eeb6f06-3d0d-4a34-b342-088ce0df80c7', '').
narrative_ontology:cs_kernel_id(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, layer_two_builders).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, feature_seeking_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, protocol_contributors).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, immutability_reliant_holders).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, full_node_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, core_protocol_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, full_node_operators).
narrative_ontology:constraint_beneficiary(bitcoin_consensus_kernel__utility_reading, miners).
narrative_ontology:constraint_victim(bitcoin_consensus_kernel__utility_reading, miners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Merge access to the reference client and stewardship of the change process sit with a small rotating set of long-tenured contributors. They review proposals, judge whether a candidate change preserves backward compatibility, and shepherd activation through the BIP process. Their standing rests on a decade of accumulated trust; leaving would mean surrendering work and reputation built inside the project, and several have declined lucrative outside offers to stay.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, core_protocol_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, core_protocol_maintainers, beneficiary).

% Grant-funded, corporate-employed, and volunteer developers whose livelihoods and reputations ride on a continuing improvement mandate. They write proposals, run tests, and staff review. A permanently frozen protocol would shrink the work available to this ecosystem; their skills port to other chains, so their attachment is professional rather than existential.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, protocol_contributors, beneficiary,
    organized, biographical, mobile, global).

% Build payment channels, sidechains, and other extensions that treat the base protocol as a substrate to extend rather than a finished product. Each accepted base-layer improvement enlarges what they can build; a frozen base would strand parts of their roadmap. Capital and talent can relocate to other ecosystems if this one closes.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, layer_two_builders, beneficiary,
    moderate, biographical, mobile, global).

% Users and businesses that adopted the system for what it can become as much as for what it is: privacy tooling, richer scripting, scaling headroom. They gain capability with each accepted improvement and can move assets elsewhere if the roadmap disappoints.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, feature_seeking_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Hold the asset primarily as a long-horizon store of value whose worth depends on the rules never changing underneath them. Every accepted protocol change, however backward-compatible, reduces the certainty they priced in when adopting. Selling out means realizing the position and abandoning the thesis that motivated it, so their practical response is argument and organization rather than exit.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, immutability_reliant_holders, payer,
    moderate, generational, identity_locked, global).

% Run validating software as their contribution to the system's trustlessness. Each change obliges them to upgrade or silently fall out of consensus, and the accumulating feature surface raises their verification burden over time. Stopping is cheap; staying current is the price of their voice counting.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, full_node_operators, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, full_node_operators, beneficiary).

% Operate the hash power that activates or blocks changes through signaling. Upgrades impose testing and coordination costs and periodically threaten fee expectations, but network growth from successful improvements raises the value of what they secure. Hardware has partial resale and alternative-coin use, so their attachment to these exact rules is economic, not total.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, miners, payer,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_consensus_kernel__utility_reading, miners, beneficiary).

% Advocates who read the founding document as a completed covenant rather than a draft. They publish objections to every proposed change and organize ossification campaigns, but the activation process counts hash signaling and node adoption, not interpretive authority, so their objection channel has no formal seat in decisions.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, maximalist_faction, excluded,
    organized, generational, identity_locked, global).

% Researchers and commentators who map incentive structures and protocol politics. They take no side in activation and bear none of its costs; their output informs the other seats.
narrative_ontology:constraint_stakeholder(bitcoin_consensus_kernel__utility_reading, cryptoeconomic_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_consensus_kernel__utility_reading, protocol_contributors).
narrative_ontology:fixing_cost_class(bitcoin_consensus_kernel__utility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the maintenance problem unique to leaderless consensus systems: how a decentralized network deploys necessary changes (bug fixes, capacity, new capability) without requiring unanimity and without splitting into incompatible ledgers. The whitepaper's mechanism supplies the shared baseline everyone already validates; backward-compatible extension lets a supermajority upgrade while laggards keep validating.
% TRANSFER_FUNCTION: Moves rule-certainty from long-horizon holders to the development commons, since each accepted change converts guaranteed stasis into optionality; moves upgrade and verification work onto node and infrastructure operators; moves development authority, attention, and funding toward the contributor ecosystem; moves new capability outward to adopters and builders.
% ABSENT_VOICES: Strict-constructionist holders and the ossification advocacy current object that each change dilutes a covenant they adopted under; they are vocally present in discourse but hold no seat in activation, which counts hash signaling and node adoption instead. Future users not yet in the system will live under rules chosen now and have no representative. Infrastructure providers affected by change timing participate through informal channels rather than any defined seat.
% DISAPPEARANCE_RATIONALE: If the floor-not-ceiling norm vanished overnight and the whitepaper were treated as a complete, closed specification, security patches and capacity fixes would each demand extraordinary renegotiation, the layer-2 build-out would lose its legitimating anchor, and competing interpretations would harden into rival camps as they briefly did during the blocksize conflict. The development commons would fracture or atrophy.
% FOUNDING_PROBLEM: Bootstrapping peer-to-peer electronic cash without trusted intermediaries: the whitepaper shipped a minimum viable proof-of-work consensus mechanism adequate to launch, explicitly leaving refinements to later versions rather than waiting for a perfect design.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the archived early record (release notes, mailing-list posts, the shipped source's own versioning) attests that the launched mechanism was treated by its first maintainers as a draft subject to revision. But the same archival corpus contains passages invoked by the rival reading, and no disinterested party attests that the text unambiguously establishes evolvability; what the archive corroborates is the ambiguity itself.
narrative_ontology:disappearance_verdict(bitcoin_consensus_kernel__utility_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_consensus_kernel__utility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_consensus_kernel__utility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_consensus_kernel__utility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_consensus_kernel__utility_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_consensus_kernel__utility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_consensus_kernel__utility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_consensus_kernel__utility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope on structural grounds: a genuine coordination function (split-free deployment of necessary changes in a leaderless network — no rival mechanism replicates it) coexists with asymmetric transfer (holder certainty and operator burden fund builder optionality), held together by active enforcement (supermajority activation thresholds, reference-client gatekeeping). Metrics are authored from descriptive operation: extractiveness 0.48 is moderate — real transfers occur but the coordination delivered is substantial; suppression 0.35 reflects enforcement that is real but porous, since the 2017 split proved exit survivable; theater_ratio 0.15 is low because the process ships working code, with a slow ritual accretion around annual debate cycles; accessibility_collapse 0.30 because alternatives persist (rival chains, forks, layered workarounds); resistance 0.55 because every change proposal meets organized ossification pushback. The temporal series share one seven-point grid (unit: years since genesis; t0=2009, t3=2012, t6=2015, t8=2017, t10=2019, t13=2022, t17=2026). The suppression_requirement series is deliberately non-monotonic: enforcement machinery ratcheted up through the blocksize conflict (peak at t8, the UASF/NYA/split year) and partially decayed after settlement — this is an enforcement-capacity trajectory, not noise. Coalition check: the victim seats are not helpless — the 2017 UASF demonstrated that node operators and holders can coordinate against miner preferences, which caps achievable suppression and is reflected in the post-peak decay. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the builder and adopter seats the arrangement presents as enabling coordination — the floor is what makes building possible. From the immutability-reliant holder seat the same structure operates as a recurring levy on certainty they thought they had purchased; from the node-operator seat it is an upgrade treadmill. From the maintainer seat it is stewardship. The engine computes these per-seat classifications from the structural data; the divergence between the beneficiary-side and payer-side computations is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (layer_two_builders, feature_seeking_adopters, protocol_contributors) derive low directionality for those seats — the arrangement subsidizes them. Victim declarations (immutability_reliant_holders, full_node_operators) derive high directionality, amplified for holders by identity_locked exit: their thesis binds them more tightly than any lock-in contract could, sitting them near the full-target end despite moderate class power. Dual-positioned agents (miners, node operators carrying secondary beneficiary roles) sit midrange — they both pay and collect. The excluded maximalist faction would derive high directionality but contributes no seat to activation. Global spatial scope scales effective extraction modestly upward for the target seats; suppression remains unscaled by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bootstrap a working peer-to-peer cash system — is substantially accomplished, yet the arrangement persists with a transformed function: perpetual stewardship of an evolving system. Because the status is contested rather than dead (parties genuinely dispute whether the mandate was 'build the thing' or 'steward it forever'), the dead-mandate-plus-world_rearranges mismatch does not fire, and no zombie flag is warranted. The classification guards both mislabeling errors: a pure-rope reading would erase the real transfer borne by holders and operators behind the coordination story; a pure-snare reading would erase the genuine split-free-upgrade coordination that no alternative mechanism provides and that the 2017 fork's stagnation relative to the main chain partially vindicates. Tangled rope keeps both halves visible and lets the temporal series show which half is growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of bitcoin_consensus_kernel; how would the maximalist_reading or pragmatic_synthesis restructure the beneficiary/victim sets and epsilon for the same underlying arrangement?',
    'Comparative classification across the three sibling stories once all are compiled; divergence in computed types and victim sets localizes where the readings actually disagree structurally.',
    'Under the maximalist reading the polarity inverts: improvers become covenant-violators and holders become the protected class, with epsilon collapsing toward coordination-only. Under the pragmatic synthesis, extraction concentrates solely on attempted base-layer monetary changes and the victim set shrinks accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed restructuring of the same arrangement under sibling readings of the consensus-kernel.').

omega_variable(
    ossification_guarantee_valuation,
    'What is the actual per-change cost borne by immutability_reliant_holders — how much certainty premium does each accepted protocol change destroy, and does it persist?',
    'Event studies around soft-fork activations measuring long-horizon holder behavior, any detectable immutability premium in valuation, and whether the cost concentrates before activation and vanishes after.',
    'Near-zero persistent cost pushes the arrangement toward pure rope; a material persistent cost confirms tangled_rope; costs that concentrate pre-activation and dissolve after would indicate transitional, scaffold-flavored extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_guarantee_valuation, empirical, 'Magnitude and persistence of the certainty cost transferred from long-horizon holders.').

omega_variable(
    enforcement_authority_source,
    'Is the evolvability norm enforced by technical activation thresholds (structural) or by maintainer social authority (reputational, internalized by contributors)?',
    'Counterfactual cases: instances where miners or nodes activated changes maintainers opposed, or refused changes they endorsed; the frequency of such overrides measures how much of the enforcement is code versus standing.',
    'Predominantly structural enforcement keeps suppression as authored; predominantly reputational enforcement implies part of the measured suppression is internalized and fragile to maintainer turnover, raising the internalization share documented in the omega apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_authority_source, empirical, 'Structural versus reputational basis of the norm''s enforcement machinery.').

omega_variable(
    layer2_success_dependency,
    'Does this reading''s stability depend on continued layer-2 delivery — would extended stagnation in layered capacity shift adherents toward the sibling readings?',
    'Track adherence indicators (developer allocation, grant flows, discourse positioning) through periods of layer-2 shortfall versus delivery.',
    'Layer-2 failure would erode the floor-reading''s beneficiary base, raise effective extraction on remaining participants, and push the arrangement''s trajectory toward degraded inertial dynamics; sustained delivery stabilizes the tangled_rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer2_success_dependency, empirical, 'Dependence of the utility reading''s viability on the layer-2 build-out it legitimates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_kernel__utility_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_consensus_kernel__utility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_consensus_kernel__utility_reading, theater_ratio, 3, 0.07).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_consensus_kernel__utility_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_consensus_kernel__utility_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_consensus_kernel__utility_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(bitc_tr_t13, bitcoin_consensus_kernel__utility_reading, theater_ratio, 13, 0.14).
narrative_ontology:measurement(bitc_tr_t17, bitcoin_consensus_kernel__utility_reading, theater_ratio, 17, 0.15).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bitc_be_t3, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(bitc_be_t8, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(bitc_be_t10, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(bitc_be_t13, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 13, 0.46).
narrative_ontology:measurement(bitc_be_t17, bitcoin_consensus_kernel__utility_reading, base_extractiveness, 17, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t3, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 3, 0.16).
narrative_ontology:measurement(bitc_su_t6, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(bitc_su_t8, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(bitc_su_t10, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(bitc_su_t13, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 13, 0.36).
narrative_ontology:measurement(bitc_su_t17, bitcoin_consensus_kernel__utility_reading, suppression_requirement, 17, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_consensus_kernel__utility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__maximalist_reading).
narrative_ontology:affects_constraint(bitcoin_consensus_kernel__utility_reading, bitcoin_consensus_kernel__pragmatic_synthesis).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what the whitepaper establishes' covers three structurally distinct claims with different epsilon values, beneficiary/victim structures, and failure modes. The maximalist reading (whitepaper as immutable covenant) is historically upstream and is cited as authority by opponents of every change; the pragmatic synthesis (immutable base rules, innovative upper layers) mediates; this utility reading (whitepaper as minimum viable baseline) is instantiated here as a separate file with its own stable epsilon. Sibling files link back via their own network.affects_constraints entries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
