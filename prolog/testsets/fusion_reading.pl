% ============================================================================
% CONSTRAINT STORY: fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fusion_reading, []).

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
 *   constraint_id: fusion_reading
 *   human_readable: Money-Governance Fusion: Stake-Weighted Consensus as Permanent Electorate
 *   domain: constitutional political economy / blockchain governance
 *
 * SUMMARY:
 *   This story instantiates the FUSION reading of the money-governance
 *   coupling kernel: the claim that wealth held on-chain converts directly
 *   and structurally into voting weight or law-making power. Under this
 *   reading, stake-weighted consensus and token-voting DAOs are not merely
 *   using money as one input among several to governance — they identify
 *   capital ownership WITH political standing, such that the founding
 *   allocation compounds permanently into political power via staking rewards
 *   proportional to existing stake. This reading treats the resulting
 *   structure as fusion: a single mechanism that is simultaneously the
 *   sybil-resistance solution and the plutocracy-generating mechanism, with
 *   no principled separation between the two functions. The victim set under
 *   this reading specifically includes participants who contribute labor,
 *   use, or presence to the network but hold no capital — their voice is not
 *   merely weighted less; it is structurally priced out because the
 *   vote-weight function has no non-monetary input channel at all.
 *
 * KEY AGENTS:
 *   - founding_allocation_holders: Primary beneficiary (institutional/arbitrage) — captures compounding governance rent from genesis allocation
 *   - large_stake_validators: Agenda-setter (organized/mobile) — sets what proposals reach a vote via block production and delegation
 *   - non_wealth_participants: Primary excluded target (powerless/trapped) — labor and use without capital, structurally without voting channel
 *   - protocol_designers: Analytical observer (analytical/analytical) — documents but cannot alter deployed fusion mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fusion_reading, 0.71).
domain_priors:suppression_score(fusion_reading, 0.78).
domain_priors:theater_ratio(fusion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fusion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fusion_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fusion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fusion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fusion_reading, tangled_rope).
narrative_ontology:human_readable(fusion_reading, "Money-Governance Fusion: Stake-Weighted Consensus as Permanent Electorate").
narrative_ontology:topic_domain(fusion_reading, "constitutional political economy / blockchain governance").

domain_priors:requires_active_enforcement(fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fusion_reading, 'db348a76-c800-4c56-9a6d-e9f365a5ff0b').
narrative_ontology:cs_kernel_codification('db348a76-c800-4c56-9a6d-e9f365a5ff0b', formalized).
narrative_ontology:cs_authority_grounding('db348a76-c800-4c56-9a6d-e9f365a5ff0b', extraction).
narrative_ontology:cs_interpretation_layer_present('db348a76-c800-4c56-9a6d-e9f365a5ff0b').
narrative_ontology:cs_reading_relation('db348a76-c800-4c56-9a6d-e9f365a5ff0b', money_governance_coupling__exile_reading, forecloses).
narrative_ontology:cs_reading_relation('db348a76-c800-4c56-9a6d-e9f365a5ff0b', money_governance_coupling__adjacency_reading, influences).
narrative_ontology:cs_axiom('db348a76-c800-4c56-9a6d-e9f365a5ff0b', foundational, capital_risk_bearing_grounds_permanent_voice).
narrative_ontology:cs_axiom_status(capital_risk_bearing_grounds_permanent_voice, holdable).
narrative_ontology:cs_axiom_grounding('db348a76-c800-4c56-9a6d-e9f365a5ff0b', capital_risk_bearing_grounds_permanent_voice, instrumental).
narrative_ontology:cs_axiom('db348a76-c800-4c56-9a6d-e9f365a5ff0b', secondary, sybil_resistance_requires_undecayed_stake_weighting).
narrative_ontology:cs_axiom_status(sybil_resistance_requires_undecayed_stake_weighting, holdable).
narrative_ontology:cs_axiom_grounding('db348a76-c800-4c56-9a6d-e9f365a5ff0b', sybil_resistance_requires_undecayed_stake_weighting, empirically_contingent).
narrative_ontology:cs_reference_frame('db348a76-c800-4c56-9a6d-e9f365a5ff0b', capital_risk_bearing_legitimizes_political_weight).
narrative_ontology:cs_drift_state('db348a76-c800-4c56-9a6d-e9f365a5ff0b', post_dao_treasury_capture_controversies, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db348a76-c800-4c56-9a6d-e9f365a5ff0b', '').
narrative_ontology:cs_kernel_id(fusion_reading, money_governance_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fusion_reading, founding_allocation_holders).
narrative_ontology:constraint_beneficiary(fusion_reading, large_stake_validators).
narrative_ontology:constraint_beneficiary(fusion_reading, protocol_treasury_controllers).
narrative_ontology:constraint_victim(fusion_reading, non_wealth_participants).
narrative_ontology:constraint_victim(fusion_reading, late_joining_users).
narrative_ontology:constraint_victim(fusion_reading, labor_contributors_without_capital).
narrative_ontology:constraint_vindicates(fusion_reading, skin_in_the_game_alignment_doctrine).
narrative_ontology:constraint_vindicates(fusion_reading, sybil_resistance_necessity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Received disproportionate token allocations at genesis (pre-mine, founder grants, early investor rounds). Because voting weight is denominated in stake, their initial capital position converts directly and permanently into law-making power over protocol upgrades, treasury disbursement, and fee structures. They can sell, delegate, or restake without losing standing, and their share compounds through staking rewards, which are themselves proportional to existing stake.
narrative_ontology:constraint_stakeholder(fusion_reading, founding_allocation_holders, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fusion_reading, founding_allocation_holders, agenda_setter).

% Operate validator infrastructure sized to their capital; they propose blocks, ratify governance proposals, and set the effective agenda of what upgrades even reach a vote. Their exit option is real (they can migrate stake to a competing chain) but this mobility is precisely what disciplines the protocol toward serving large holders rather than the median participant.
narrative_ontology:constraint_stakeholder(fusion_reading, large_stake_validators, agenda_setter,
    organized, generational, mobile, global).

% Multisig or DAO-elected bodies that disburse treasury funds; election to these seats is itself token-weighted, so control over the money supply and control over who controls the money supply are the same fused mechanism. They benefit from the fusion's self-reinforcing character.
narrative_ontology:constraint_stakeholder(fusion_reading, protocol_treasury_controllers, beneficiary,
    institutional, generational, arbitrage, global).

% Contribute labor, code, community moderation, or usage to the network but hold negligible stake. Their preferences can be expressed only through voice channels (forums, social pressure) that carry no binding weight against a stake-weighted vote. They cannot buy in without capital they do not have, and cannot vote-signal their way around the fusion.
narrative_ontology:constraint_stakeholder(fusion_reading, non_wealth_participants, excluded,
    powerless, biographical, trapped, global).

% Acquire tokens after genesis, typically at prices reflecting the accumulated network effects the founding cohort already captured governance rents from. Their per-dollar voting weight is identical in principle to an early holder's, but they need vastly more capital to reach equivalent influence, effectively pricing out political participation for anyone who was not present at allocation time.
narrative_ontology:constraint_stakeholder(fusion_reading, late_joining_users, payer,
    moderate, biographical, constrained, global).

% Developers, translators, node-support volunteers, and community organizers whose work sustains the protocol's value but who are compensated (if at all) in ways that do not convert into commensurate governance weight. Their structural position is labor without capital in a system where only capital votes.
narrative_ontology:constraint_stakeholder(fusion_reading, labor_contributors_without_capital, payer,
    powerless, biographical, trapped, global).

% Academic and applied cryptoeconomics researchers who study whether stake-weighted governance produces plutocratic capture or genuine sybil-resistant coordination. They can document the pattern but hold no lever to alter deployed protocols unless their analysis is adopted by a future fork.
narrative_ontology:constraint_stakeholder(fusion_reading, protocol_designers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fusion_reading, founding_allocation_holders).
narrative_ontology:fixing_cost_class(fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stake-weighted consensus solves a genuine sybil-resistance problem: without some costly signal tied to identity, an attacker could manufacture unlimited voting identities for free. Requiring economic stake to vote or validate makes identity-manufacture costly and lets the protocol reach agreement without a trusted central authority.
% TRANSFER_FUNCTION: Moves political agenda-setting power and treasury control from anyone who might otherwise have a claim (users, laborers, later entrants) to whoever already holds capital, and further concentrates it over time because staking rewards are paid in proportion to existing stake, compounding the founding allocation into permanent structural advantage.
% ABSENT_VOICES: Non-wealth-holding contributors and late-joining users would object that governance weight tracks capital rather than contribution, use, or stake in outcomes broadly construed (as opposed to token stake narrowly construed) — but the voting mechanism itself has no channel for this objection to register as a vote; it can only appear as off-chain social pressure, which the on-chain mechanism is not obligated to honor.
% DISAPPEARANCE_RATIONALE: If money-governance fusion were removed and replaced with a one-participant-one-vote or contribution-weighted scheme, treasury allocation, protocol upgrade paths, and validator economics would all reorganize substantially — the current beneficiary class would lose its compounding structural advantage and would resist the change accordingly, which is itself evidence of how much is currently riding on the fusion.
% FOUNDING_PROBLEM: Permissionless networks needed a way to reach consensus and allocate scarce coordination rights (block production, governance weight) without a trusted central party, in an environment where anyone could otherwise fabricate unlimited free identities (the Sybil problem).
% FOUNDING_PROBLEM_CORROBORATION: Protocol designers and independent cryptoeconomics researchers attest the sybil-resistance problem is real and durable — but the same researchers, along with non-wealth contributors, attest that the specific solution chosen (permanently fusing capital and political weight rather than, e.g., time-decaying stake, quadratic weighting, or identity-based schemes) was a design choice that also serves the founding allocation holders' ongoing capture, not a forced solution. The founding allocation holders themselves are the primary source asserting the fusion is the only or best solution.
narrative_ontology:disappearance_verdict(fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fusion_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) reflects the compounding character of stake-proportional rewards: the mechanism does not merely give capital a voice, it gives capital a growing voice over time relative to labor or late entry, which is a structural rent rather than a one-time allocation effect. Suppression (0.78) is high because alternative governance forms — one-participant-one-vote, quadratic voting, contribution-weighted schemes — are foreclosed once a chain's social and technical infrastructure locks into stake-weighting; forking away is technically possible but carries prohibitive coordination costs that only large, already-advantaged holders can bear, which itself reproduces the fusion's asymmetry at the exit layer. Theater ratio (0.42) is moderate-rising: governance forums and community calls perform deliberation, but binding outcomes track stake regardless of forum sentiment, and this gap widens over the measured interval as governance theater becomes more elaborate while its substantive bindingness does not increase.
 *
 * PERSPECTIVAL GAP:
 *   From the founding-allocation seat, stake-weighted governance is a rope: coordination that solves sybil-resistance efficiently, and the compounding reward is simply the market correctly pricing early risk-bearing. From the non-wealth-participant seat, the identical mechanism is a tangled rope shading toward snare: a genuine coordination problem (sybil resistance) is being used as cover for a permanent capture of political voice by whoever arrived first with capital. The engine's structural inputs — beneficiary/victim declarations, enforcement requirement, compounding reward structure — are what produce this divergence; the fusion reading asserts this divergence is intrinsic to the mechanism itself, not an artifact of observation.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding allocation holders and large validators sit near the full-beneficiary end: they set the rules, collect the compounding reward, and retain exit options (arbitrage/mobile) that let them discipline the protocol toward their preferences without bearing the cost of protocol failure symmetrically with others. Non-wealth participants and labor contributors sit near the full-target end: trapped exit options (their sunk contribution has no capital-equivalent value in the vote), and the extraction they bear is diffuse but real — foreclosed political voice, not a directly billed cost. Late-joining users occupy an intermediate position: moderate power, constrained exit, paying a capital premium for governance weight that founders received for free.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sybil resistance in a permissionless network) remains partially live — the technical need for costly identity signals has not disappeared. But under the fusion reading, the SPECIFIC solution chosen (permanent capital-political fusion via compounding stake rewards) is not required by the founding problem; alternative sybil-resistant designs exist (bonded identity, time-decaying stake, quadratic funding) that do not fuse wealth and political power as tightly. The persistence of the fusion form specifically, rather than sybil-resistance generally, is best explained by its service to the beneficiary class rather than by continued necessity — a classic mandatrophy signature: the founding problem partially survives, but the mechanism retained exceeds what the surviving problem requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fusion_vs_adjacency_boundary_location,
    'Is the compounding, permanent character of stake-weighted governance rewards an intrinsic requirement of sybil-resistant consensus, or a specific and separable design choice that the adjacency reading shows is avoidable (e.g. via decaying stake weight or capped voting power)?',
    'Comparative protocol analysis: examine deployed systems using bounded or decaying stake-weight schemes and measure whether they achieve comparable sybil-resistance without comparable governance concentration over multi-year intervals.',
    'If bounded/decaying schemes achieve equivalent sybil-resistance, the fusion reading''s core premise — that fusion is required, not merely chosen — is undermined, and this constraint''s classification would shift toward a more clearly extractive reading (snare) since the coordination justification would no longer require the specific extractive mechanism observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_vs_adjacency_boundary_location, conceptual, 'Whether fusion''s compounding character is structurally necessary or a contingent design choice distinguishing it from the adjacency reading.').

omega_variable(
    founding_allocation_naturalness,
    'Is the founding allocation''s conversion into permanent political weight best understood as a natural consequence of first-mover risk-bearing (as founders claim), or as a constructed advantage that the protocol''s own governance mechanism then entrenches and could, in principle, unwind?',
    'Examine hard-fork or governance-vote history: has any stake-weighted system''s own governance process ever voted to redistribute or decay founding advantage? If never, and if attempts are structurally blocked by the same stake-weighting, that is evidence for constructed-and-entrenched rather than natural-and-earned.',
    'If entrenchment is confirmed as self-protecting rather than natural, this strengthens the case that fusion functions as tangled_rope-shading-to-snare rather than genuine coordination; if founding advantage regularly gets voted down or decayed, the fusion reading''s extraction claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_allocation_naturalness, empirical, 'Whether founder capital-to-power conversion is a natural consequence of risk-bearing or a self-entrenching constructed advantage.').

omega_variable(
    exit_cost_asymmetry_measurement,
    'How prohibitive, in practice, is the cost of forking away from a fused-governance chain for non-wealth participants versus for large stakeholders?',
    'Case studies of historical contentious forks (e.g. governance disputes leading to chain splits): measure the capital and coordination cost borne by each stakeholder class and whether non-wealth participants'' interests were represented in the resulting fork''s design.',
    'If exit costs are shown to be dramatically asymmetric (large holders exit cheaply, small/no-capital participants cannot meaningfully exit at all), this corroborates the trapped exit_options classification for non_wealth_participants and labor_contributors_without_capital and supports the high suppression score.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_asymmetry_measurement, empirical, 'Whether fork/exit costs are asymmetric across stakeholder capital classes, corroborating the trapped-exit classification for non-capital participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fusion_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fusi_tr_t0, fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fusi_tr_t6, fusion_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(fusi_tr_t12, fusion_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(fusi_tr_t18, fusion_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement(fusi_tr_t24, fusion_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(fusi_tr_t30, fusion_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(fusi_tr_t36, fusion_reading, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(fusi_be_t0, fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fusi_be_t6, fusion_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(fusi_be_t12, fusion_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(fusi_be_t18, fusion_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(fusi_be_t24, fusion_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(fusi_be_t30, fusion_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(fusi_be_t36, fusion_reading, base_extractiveness, 36, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fusi_su_t0, fusion_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fusi_su_t6, fusion_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(fusi_su_t12, fusion_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(fusi_su_t18, fusion_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(fusi_su_t24, fusion_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(fusi_su_t30, fusion_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(fusi_su_t36, fusion_reading, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fusion_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fusion_reading, 0.15).
narrative_ontology:affects_constraint(fusion_reading, exile_reading).
narrative_ontology:affects_constraint(fusion_reading, adjacency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the money_governance_coupling kernel. exile_reading holds money and governance should be categorically separated (forecloses fusion's premise that stake can legitimately ground vote weight). adjacency_reading holds capital can bound or inform political weight without full, compounding fusion (coexists with fusion as an alternative design space; influences fusion by demonstrating a less extractive alternative is available, which raises the bar for what fusion must justify). Each reading has a distinct ε: fusion is authored here as substantially extractive and hardening (ε rising 0.42→0.71 over the interval); the sibling readings are expected to show different ε profiles reflecting their different structural commitments. Do not average across the three files — each is a self-contained, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
