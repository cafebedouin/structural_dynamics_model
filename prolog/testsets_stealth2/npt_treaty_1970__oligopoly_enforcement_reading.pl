% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Regime under the Oligopoly Enforcement Reading (Articles I-II Binding, Article VI Aspirational)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The NPT text has been fixed since 1968; what varies across parties is the
 *   legal character assigned to its articles. This story instantiates the
 *   oligopoly enforcement reading: Articles I-II (no transfer, no
 *   acquisition) carry primary binding force and are actively enforced
 *   through Agency safeguards, export-control gatekeeping, and Council
 *   action, while Article VI (negotiations toward disarmament) is treated as
 *   contingent and aspirational - restated at each review conference, never
 *   justiciable, never dated. The resulting arrangement is a maintained
 *   oligopoly: five recognized arsenals sit wholly outside verification,
 *   every other party's civilian fuel cycle is opened to continuous
 *   inspection, and the recognized five control the enforcement organ by
 *   veto. A genuine coordination function (cascade prevention, a single
 *   standard for peaceful nuclear trade, assurance that lets allies accept
 *   protector arsenals) coexists with a starkly asymmetric burden structure.
 *   The claim and the metrics are independent authored facts: the type is
 *   stated from what is structurally true of this reading's arrangement, the
 *   metrics from what descriptively happens. The sibling readings are
 *   separate constraint stories with their own epsilon values and are linked
 *   through the network section; nothing about them is averaged into this
 *   file.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: Primary beneficiary and agenda-setter (institutional/arbitrage) - collects status permanence, unaccountable arsenals, and enforcement control
 *   - nnws_parties_under_safeguards: Primary target (moderate/constrained) - bears the entire verification burden and the foregone weapons option
 *   - threshold_latent_weapons_states: Secondary target (powerful/constrained) - denied the deterrent option the recognized five retain indefinitely
 *   - extended_deterrence_umbrella_states: Secondary beneficiary (powerful/constrained) - buys protection without arming, supports the rules that suppress neighbors' programs
 *   - iaea_secretariat: Enforcement administrator (institutional/constrained) - verifies only what the recognized five permit, funded by the states it monitors
 *   - nonaligned_review_conference_bloc: Organized internal opposition (organized/constrained) - wins consensus language, never binding change
 *   - nonparty_nuclear_armed_states: Excluded seat (powerful/mobile) - outside the categories, arsenal intact, accommodated by trade
 *   - dprk_withdrawn_state: Excluded seat (moderate/mobile) - exercised the exit clause, paid in sanctions, completed an arsenal
 *   - disarmament_advocacy_networks: Analytical observer (analytical/analytical) - documents the asymmetry, builds the rival prohibition instrument, holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.64).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Regime under the Oligopoly Enforcement Reading (Articles I-II Binding, Article VI Aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'da039457-c801-4707-9ef1-98c36b4f4afa').
narrative_ontology:cs_kernel_codification('da039457-c801-4707-9ef1-98c36b4f4afa', fixed_text).
narrative_ontology:cs_authority_grounding('da039457-c801-4707-9ef1-98c36b4f4afa', extraction).
narrative_ontology:cs_interpretation_layer_present('da039457-c801-4707-9ef1-98c36b4f4afa').
narrative_ontology:cs_reading_relation('da039457-c801-4707-9ef1-98c36b4f4afa', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_reading_relation('da039457-c801-4707-9ef1-98c36b4f4afa', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('da039457-c801-4707-9ef1-98c36b4f4afa', foundational, horizontal_proliferation_is_primary_binding_obligation).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('da039457-c801-4707-9ef1-98c36b4f4afa', horizontal_proliferation_is_primary_binding_obligation, conventional).
narrative_ontology:cs_axiom('da039457-c801-4707-9ef1-98c36b4f4afa', foundational, vertical_disarmament_obligation_is_aspirational_not_justiciable).
narrative_ontology:cs_axiom_status(vertical_disarmament_obligation_is_aspirational_not_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('da039457-c801-4707-9ef1-98c36b4f4afa', vertical_disarmament_obligation_is_aspirational_not_justiciable, instrumental).
narrative_ontology:cs_reference_frame('da039457-c801-4707-9ef1-98c36b4f4afa', horizontal_nonproliferation_primacy).
narrative_ontology:cs_drift_state('da039457-c801-4707-9ef1-98c36b4f4afa', post_indefinite_extension_humanitarian_initiative_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('da039457-c801-4707-9ef1-98c36b4f4afa', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, extended_deterrence_umbrella_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties_under_safeguards).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_latent_weapons_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nonaligned_review_conference_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, Russia, the United Kingdom, France, and China hold permanent arsenals that sit outside any comprehensive verification arrangement, control Security Council enforcement through their vetoes, anchor the export-control arrangements, and preside over the review process in which disarmament commitments are periodically restated and deferred. Their status category became legally permanent in 1995. Nothing in the arrangement compels an accounting of their arsenals, and leaving the categories they define has no meaning for them.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Non-nuclear allies under American, British, or French security guarantees - NATO members, Japan, South Korea, Australia - receive protection without operating weapons programs of their own. The arrangement suppresses their neighbors' weapons options, sparing them regional arms races. Their security planning is fused with the protector's arsenal; ending the dependence would mean starting indigenous programs that collide with the rules they currently uphold.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, extended_deterrence_umbrella_states, beneficiary,
    powerful, biographical, constrained, continental).

% The broad body of non-nuclear parties opens its civilian nuclear facilities to Agency inspection under comprehensive safeguards agreements, and since 1997 many have accepted the Additional Protocol's wider declaration and access burdens. They carry inspection costs, expose commercially sensitive plant data, and live under compliance findings that can trigger Council action. Their arsenal-free status is verified continuously; the recognized arsenals are verified never. Withdrawal is legally available but carries sanctions, supply cutoff, and isolation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties_under_safeguards, payer,
    moderate, generational, constrained, global).

% States such as Japan, South Korea, Germany, and Brazil hold the enrichment, reprocessing, metallurgy, and delivery expertise to assemble weapons within months to years. They forgo that option and manage their threat environments through alliances and latent capability instead. The option they surrendered stays permanently open to the five recognized holders. Their hedging - plutonium stocks, enrichment bids, dual-use research - draws scrutiny the recognized arsenals never face.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_latent_weapons_states, payer,
    powerful, biographical, constrained, regional).

% The Agency designs and runs the inspection system, but only where member states grant access: its comprehensive safeguards reach non-nuclear parties' civilian fuel cycles, never the recognized arsenals' military complexes. Its budget depends on the same member states it monitors, its findings travel to a Council whose permanent members can veto action, and its mandate exists only so long as the arrangement it services continues.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% The Non-Aligned Movement and the New Agenda Coalition coordinate common positions before and during the quinquennial review conferences, pressing for dated disarmament benchmarks, negative security assurances, and universalization. They have repeatedly secured consensus language acknowledging the disarmament pillar and repeatedly watched it produce no binding change. Between conferences they continue paying their compliance costs.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nonaligned_review_conference_bloc, payer,
    organized, generational, constrained, global).

% India, Pakistan, and Israel never signed and hold arsenals entirely outside the arrangement's obligations. They faced supply denial through the export-control arrangements until accommodations were negotiated - India's 2008 exemption most prominently. They had no seat when the bargain was struck in 1968 and reject its category system while trading extensively with its members.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nonparty_nuclear_armed_states, excluded,
    powerful, generational, mobile, regional).

% North Korea announced withdrawal under the exit article in 2003, tested devices from 2006 onward, and now operates outside the arrangement under United Nations sanctions. Its trajectory prices the exit option: survivable, isolating, and followed by arsenal completion. Other members cite it both as proof the rules bind and as proof they can be walked away from.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, dprk_withdrawn_state, excluded,
    moderate, biographical, mobile, regional).

% Academic analysts, humanitarian-initiative campaigners, and prohibition-treaty advocates document the burden asymmetry and mobilize around the weapons' humanitarian consequences, culminating in the 2017 prohibition treaty that the recognized-arsenal holders boycott. They hold no enforcement power and attend the review process as observers.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, disarmament_advocacy_networks, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: unchecked, each new weapons state triggers its neighbors' programs - the cascade forecast in the early 1960s - so a single membership category system, one verification standard for peaceful nuclear trade, and shared export gatekeeping let states buy security assurance more cheaply than arming. It also stabilizes expectations enough that allies accept protector arsenals instead of building their own.
% TRANSFER_FUNCTION: Moves verification access and foregone weapons options from every non-nuclear party to the recognized five; moves status permanence and enforcement control to the recognized five; moves market access and negative security assurances to compliant parties; moves the accident-and-use risk tail of arsenal maintenance to everyone within range.
% ABSENT_VOICES: The non-party weapons states had no seat when the categories were fixed in 1968 and remain outside them; the publics of the recognized five have no mechanism through which to answer for national arsenals; states that would arm if the arrangement collapsed register their preference only as compliance risk; future generations absorb the risk tail with no representative at any review conference.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the forecast cascade resumes: Gulf states follow the Iranian threshold within years, East Asian capitals revisit decades-old abstention decisions, the export-control arrangements lose their legal anchor, peaceful-trade assurances collapse, and the recognized five lose the instrument that freezes their number at five. Regional orders built on extended deterrence and non-armament would renegotiate from scratch under crisis conditions.
% FOUNDING_PROBLEM: Stop the forecast cascade - the early-1960s expectation of fifteen to twenty-five weapons states within a decade - while preserving the peaceful atom's commercial promise, by freezing the existing five-holder division and purchasing everyone else's abstention with trade access and security assurances.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Agency secretariat reporting documents active proliferation pressures (the Iranian file, the North Korean program, hedging programs in several capitals); Non-Aligned Movement review-conference statements attest the horizontal problem is live while disputing the asymmetry; the non-party states' continuing arsenal construction corroborates that demand for the option persists; the independent security-studies literature uniformly treats cascade risk as current. No source outside the recognized five attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high but bounded (0.64 at interval end): the burden asymmetry is real and structural - continuous verification on one side of a legal line, none on the other - yet most non-nuclear parties receive goods they affirm (trade access, assurance, suppressed regional arms races), which caps epsilon below snare territory. Suppression (0.58) reflects penalized rather than erased alternatives: exit is legally available and was exercised, non-membership persists, hedging continues - each at a price. Theater ratio (0.35 and rising) tracks the growing share of regime activity that is restatement rather than function: quinquennial consensus documents, disarmament-pillar language, and unequivocal undertakings that produce no binding change, against a verification core that remains real. Accessibility collapse (0.45) is far from natural-law completeness: the alternatives (withdrawal, non-membership, latency) stay visible and costly rather than unthinkable. Resistance (0.60) is sustained: one exit-and-test case, a contested program under sanctions, a permanent non-member bloc, and a rival treaty route. The measurement series run on one shared eight-point grid so every tracked metric is authored at every examined time point. Two structural steps organize the trajectories: 1995 (indefinite extension removes the reciprocity lever, lifting extraction; the Additional Protocol ratchet lifts the enforcement requirement on inspected parties while recognized-arsenal accountability stays flat) and a continued post-2010 rise as arsenal modernization proceeds alongside restated aspirations. The dynamic is monotonic drift with one step, not cyclical, so no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine derives that divergence from the structural data. From the recognized-five seat the arrangement is a public good they underwrite and police - coordination they built, defend, and staff. From the inspected-parties seat the same structure is a one-way mirror: their facilities opened, their options foreclosed, the other side of the line unverified. From the threshold-states seat it is a parity denial: the option surrendered stays permanently open to the five. From the umbrella-allies seat it is cheap security. The authored claim does not adjudicate among these; the per-seat classifications are computed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The recognized five are declared beneficiaries with arbitrage-grade exit - they wrote the categories, face no verification, and control enforcement - placing them near the full-beneficiary end, where effective extraction inverts toward subsidy. Umbrella allies are declared beneficiaries with constrained exit (alliance fusion), sitting low but above the five. Inspected non-nuclear parties and threshold states are declared victims with constrained exit - withdrawal is legal but sanction-priced - placing them near the full-target end, where effective extraction is amplified. The Agency secretariat is an agenda-setter that is neither declared beneficiary nor victim; it takes the institutional fallback near symmetry, which matches its actual position: it enforces the arrangement and its existence depends on it. No directionality overrides are needed: the two institutional seats (recognized five, Agency) differ in declared role and exit options, so the structural derivation separates them without help, and overriding at the power-atom level would collide the seats the declarations already distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - stopping the forecast cascade - is still live, so the arrangement is not mandatrophy-resolved and no zombie flag is warranted (founding_problem_status live x disappearance verdict world_rearranges produces no mismatch). The classification work is guarding the middle: calling this a pure coordination mechanism would erase the asymmetry this very reading foregrounds - the burden line, the denied deterrent, the unaccountable arsenals; calling it pure extraction would erase the catastrophe-prevention function that even the burdened seats affirm and that its disappearance test confirms (the world rearranges violently without it). The tangled-rope structure preserves both facts: a genuine coordination function that explains the arrangement's breadth and durability, and an extraction layer that explains the resistance, the rival treaty route, and the widening repudiation pressure. The drift to watch is theatrical: if the verification core ever atrophied while review-conference restatement persisted, the theater ratio would signal piton-ward decay - performance replacing function. That has not happened; the inspections remain real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_contestation,
    'This constraint instantiates the oligopoly_enforcement_reading of kernel npt_treaty_1970; how would the sibling readings restructure the beneficiary/victim sets and epsilon?',
    'Comparative classification across the three reading-level stories of the kernel. The reciprocal_disarmament_reading assigns Article VI binding force with temporal urgency, adding a vertical counterweight that shifts extraction toward the recognized-arsenal seat. The withdrawal_sovereignty_reading makes obligations contingent on security environment, converting non-nuclear compliance from owed duty to priced choice and shrinking the victim set to those who cannot exit.',
    'Under the reciprocal reading the arrangement computes with materially lower extraction on the inspected seats; under the withdrawal reading the enforcement apparatus loses its obligor base and the burden concentrates on states that stay inside. The disagreement is located in the legal character assigned to Article VI and in the conditionality of the Articles I-II obligations - not in any observable of this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_contestation, conceptual, 'Committer structure: one reading of the NPT kernel; sibling readings relocate the victim set and rebalance the bargain.').

omega_variable(
    verification_asymmetry_necessity,
    'Is the enforcement asymmetry - intrusive verification on non-nuclear parties, none on recognized arsenals - a technical necessity of any verifiable regime, or a constructed allocation serving the recognized five?',
    'Verification-theory assessment of managed-access methods for military facilities: the UK-Norway initiative, the International Partnership for Nuclear Disarmament Verification methodology work, and whether classified-design disclosure barriers are surmountable in principle.',
    'If the asymmetry is technically necessary, part of the measured extraction is irreducible coordination cost and the coordination component of the arrangement strengthens; if it is constructed, the asymmetry is rent collection and the extraction component grows accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_necessity, conceptual, 'Whether the burden asymmetry is forced by verification physics or chosen by the beneficiaries.').

omega_variable(
    indefinite_extension_lockin,
    'Did the 1995 indefinite extension remove proliferation uncertainty (a coordination gain) or remove the non-nuclear parties'' reciprocity lever (an extraction lock-in)?',
    'Counterfactual bargaining analysis using review-conference archives and Non-Aligned Movement negotiating records: compare Article VI outcomes plausibly obtainable under periodic-review renewal against the observed post-1995 record of restated-but-unimplemented disarmament commitments.',
    'If lock-in dominated, the 1995 step visible in the extractiveness series marks the transition from renewable bargain to permanent oligopoly maintenance; if coordination dominated, the step reflects the priced purchase of stability and the asymmetry was always the deal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_lockin, empirical, 'Whether permanence was bought with reciprocity or purchased stability outright.').

omega_variable(
    threshold_hedging_stability,
    'Does latent-capability hedging by threshold states stabilize the arrangement (a safety valve that vents exit pressure) or corrode it (normalizing near-weapons capability)?',
    'Track hedging-state behavior - Japanese reprocessing and separation standards, South Korean enrichment requests, Brazilian safeguards friction - against regime stress episodes; compare hedgers that endured against historical cases where hedging preceded breakout.',
    'If the safety-valve account holds, the threshold states'' burdened position is partly self-managed and their exit pressure is overstated; if the corrosion account holds, the burdened set is expanding toward open breakout and the enforcement requirement will rise further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_hedging_stability, empirical, 'Whether latency absorbs or amplifies the pressure the asymmetry generates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, tpnw_2017_humanitarian_prohibition).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the NPT'. The single treaty text is one kernel; the three readings instantiate three structurally distinct constraints with different epsilon values, because the legal character assigned to Article VI determines who owes what: under this (oligopoly enforcement) reading the arrangement is a one-way enforced bargain with the recognized five collecting status rents; under the reciprocal reading a binding Article VI counterweight lowers extraction on the inspected seats; under the withdrawal reading obligations become priced choices and the obligor base shrinks. This story is the operative enforcement reality from which the other two dissent, so it sits upstream: its enforcement asymmetry is what the reciprocal reading protests and what raises the legitimacy conditions under which the withdrawal reading's exit claims are heard. The 2017 prohibition treaty appears downstream as the repudiation vehicle built outside the benefiting parties. All family members link via affects_constraints; epsilon is stable within each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
