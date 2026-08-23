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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Two-Tier Nonproliferation Regime (Oligopoly Enforcement Reading)
 *   domain: international_law/security/regime_theory
 *
 * SUMMARY:
 *   The 1968 Non-Proliferation Treaty, in force since 1970, divides the world
 *   into five recognized nuclear weapon states and everyone else. This story
 *   instantiates the oligopoly_enforcement_reading of that kernel: Articles
 *   I-II (no transfer, no acquisition) operate as the regime's primary
 *   binding obligations, enforced through IAEA safeguards, Security Council
 *   sanctions, and supplier-cartel export controls, while Article VI
 *   (good-faith disarmament negotiation) is treated as contingent and
 *   aspirational — invoked ritually at review conferences, never justiciable.
 *   The epsilon referent is the standing arrangement under contest — the NPT
 *   regime as actually operated 1970-present — assessed by this reading's own
 *   lights: a genuine cascade-prevention coordination function wrapped around
 *   an enforcement structure whose burdens fall on disarmed states and whose
 *   benefits accrue to the weapons-holding five. The colloquial label 'the
 *   NPT bargain' decomposes, per the epsilon-invariance principle, into three
 *   structurally distinct readings (this one, reciprocal_disarmament_reading,
 *   withdrawal_sovereignty_reading) with different victim sets and different
 *   classifications; they are linked through the network layer, not merged.
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope (real coordination, real asymmetry) while the authored
 *   metrics describe substantially extractive, increasingly theatrical,
 *   actively enforced operation — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: agenda-setter and primary beneficiary (institutional/arbitrage) — administers enforcement through Security Council permanence, retains arsenals uninspected, collects the status rents of a closed club
 *   - iaea_safeguards_secretariat: administering enforcer (institutional/identity_locked) — runs the verification workload in non-weapon states, with no equivalent jurisdiction over weapon-state military programs
 *   - nnws_safeguarded_majority: primary target (organized/constrained) — bears full-scope safeguards and permanent renunciation, organizes collectively but under sanctions exposure
 *   - threshold_breakout_states: heavily positioned target with partial compensation (powerful/constrained) — denied the deterrent their latent capability would support
 *   - extended_deterrence_host_allies: secondary beneficiary (organized/constrained) — protected without ownership, invested in the two-tier division
 *   - emerging_peaceful_nuclear_programs: compensated entrants (moderate/mobile) — Article IV recipients whose alternatives are worse
 *   - tpnw_humanitarian_coalition: resisting payers (organized/constrained) — built the 2017 prohibition instrument after repeated review-cycle failures
 *   - nonparty_nuclear_armed_states: excluded armed outsiders (powerful/arbitrage) — India, Pakistan, Israel validate the discrimination critique from outside the regime
 *   - arms_control_verification_analysts: analytical observer (analytical/analytical) — supply the evidentiary record both coalitions cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.64).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.66).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Two-Tier Nonproliferation Regime (Oligopoly Enforcement Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/security/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'c79582a0-d62a-4f07-a1a4-575c0176b43a').
narrative_ontology:cs_kernel_codification('c79582a0-d62a-4f07-a1a4-575c0176b43a', fixed_text).
narrative_ontology:cs_authority_grounding('c79582a0-d62a-4f07-a1a4-575c0176b43a', extraction).
narrative_ontology:cs_interpretation_layer_present('c79582a0-d62a-4f07-a1a4-575c0176b43a').
narrative_ontology:cs_reading_relation('c79582a0-d62a-4f07-a1a4-575c0176b43a', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('c79582a0-d62a-4f07-a1a4-575c0176b43a', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('c79582a0-d62a-4f07-a1a4-575c0176b43a', foundational, articles_i_ii_unconditionally_binding).
narrative_ontology:cs_axiom_status(articles_i_ii_unconditionally_binding, holdable).
narrative_ontology:cs_axiom_grounding('c79582a0-d62a-4f07-a1a4-575c0176b43a', articles_i_ii_unconditionally_binding, conventional).
narrative_ontology:cs_axiom('c79582a0-d62a-4f07-a1a4-575c0176b43a', foundational, article_vi_nonjusticiable_aspiration).
narrative_ontology:cs_axiom_status(article_vi_nonjusticiable_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('c79582a0-d62a-4f07-a1a4-575c0176b43a', article_vi_nonjusticiable_aspiration, conventional).
narrative_ontology:cs_reference_frame('c79582a0-d62a-4f07-a1a4-575c0176b43a', horizontal_binding_vertical_deferred).
narrative_ontology:cs_drift_state('c79582a0-d62a-4f07-a1a4-575c0176b43a', post_tpnw_entry_into_force, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c79582a0-d62a-4f07-a1a4-575c0176b43a', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, extended_deterrence_host_allies).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, emerging_peaceful_nuclear_programs).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_safeguarded_majority).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_breakout_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nnws_safeguarded_majority).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, threshold_breakout_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, tpnw_humanitarian_coalition).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, possessor_club_stability_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, declared_materials_verification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear weapon states administer the regime's coercive machinery through their permanent Security Council seats, decide when safeguards disputes trigger sanctions, and control the interpretive content of Article VI at review conferences. They retain and modernize arsenals with no inspection of their own military programs, extend deterrence to allies, and collect the standing of a closed membership whose door the treaty locks behind them. Their privileged position depends on the text staying frozen; they arbitrage between enforcing the articles on others and reinterpreting the disarmament article for themselves.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary).

% Administers comprehensive safeguards and Additional Protocol inspections in non-weapon states, certifies compliance, and refers unresolved cases toward the Security Council. Its budget, mandate, and professional identity are constituted by the verification workload the regime assigns it, and it holds no equivalent audit jurisdiction over the recognized weapon states' military programs. Shrinking the safeguards mission would dissolve the organization's core purpose, so its institutional self-concept is fused with the arrangement it polices.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% The roughly 180 non-weapon states parties submit full-scope safeguards, accept intrusive inspections, and forgo the weapons option permanently. In exchange they receive peaceful-technology cooperation and the security value of neighbors' verified restraint. They organize collectively through the Non-Aligned Movement caucus and review-conference blocs to demand disarmament movement, but their leverage is bounded by sanctions exposure and alliance dependencies; leaving carries the North Korea precedent of punitive isolation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_safeguarded_majority, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, nnws_safeguarded_majority, beneficiary).

% Economically and technologically advanced states — Japan, South Korea, Germany and peers — with latent weapons capability measured in months to years of breakout time. They bear the heaviest inspection intensity relative to their industrial capacity, permanently surrender the deterrent option their capability would support, and absorb neighborhood threats from armed states outside the treaty. Their compensation runs through extended-deterrence umbrellas whose credibility they cannot independently verify, plus first-tier standing in the peaceful nuclear order. Exiting would mean choosing between arming alone against proliferated neighborhoods and forfeiting alliance protection.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_breakout_states, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, threshold_breakout_states, beneficiary).

% States under United States nuclear umbrellas receive deterrent protection without owning weapons or hosting operational arsenals, riding on the weapon states' retained forces. Decades of defense planning, basing arrangements, and public opinion in these states presuppose the two-tier division; abandoning it would force independent deterrent decisions they have politically declined for generations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, extended_deterrence_host_allies, beneficiary,
    organized, biographical, constrained, regional).

% Developing states building first reactors receive technology transfer, fuel-supply assurances, and safety training under the treaty's cooperation article — the tangible side of the bargain. Participation is conditioned on safeguards acceptance, and the alternative (exclusion from the supplier cartel) is materially worse, which keeps them inside by interest rather than attachment.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, emerging_peaceful_nuclear_programs, beneficiary,
    moderate, biographical, mobile, national).

% Over 120 mostly non-weapon states concluded after repeated review-cycle failures that the disarmament half of the bargain would not arrive on any schedule, and built an alternative instrument — the 2017 Treaty on the Prohibition of Nuclear Weapons — banning possession, use, and threat. They remain treaty parties paying safeguards costs while mounting the regime's largest institutional resistance from inside; the weapon states boycott the new instrument and use financial and alliance pressure to keep it from touching arsenal policy.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, tpnw_humanitarian_coalition, payer,
    organized, generational, constrained, global).

% India, Pakistan, and Israel stayed outside the treaty, armed, and argue that a permanent five-member weapons club proves nonproliferation law is positional rather than principled. Absent from the founding bargain, they decline both its obligations and its benefits, trading with the regime selectively — India's 2008 supplier exemption showed the regime bending when great-power interests aligned.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nonparty_nuclear_armed_states, excluded,
    powerful, civilizational, arbitrage, regional).

% Academic and think-tank specialists in regime theory and verification technology who map the bargain's reciprocity balance, document safeguards coverage gaps and arsenal modernization rates, and supply the evidentiary record that both the enforcement coalition and the disarmament coalition cite for opposite conclusions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, arms_control_verification_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the proliferation-cascade collective-action problem: a single verification standard lets states forgo weapons without fearing undetected neighbor cheating, and a capped possessor set limits the number of fingers on nuclear triggers and the number of dyads in which crises can go nuclear.
% TRANSFER_FUNCTION: Moves sovereignty over fuel-cycle decisions and submission to intrusive verification from the non-weapon majority to the P5-administered regime; moves status and security rents to the five; moves the promised disarmament obligation nominally from the weapon states to an indefinitely deferred future.
% ABSENT_VOICES: The nonparty nuclear-armed states were absent from the founding bargain and remain outside; their objection — that the structure entrenches discrimination and thereby incentivizes armament — was voiced only from outside and was answered with supplier-cartel punishment rather than argument. The Global South delegations present at founding accepted the bargain under assurance of eventual disarmament; their successors now speak through the TPNW coalition, but the seats that would insist on binding reciprocity were never given decision weight inside the enforcement machinery.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, the safeguards infrastructure would stop certifying restraint, threshold states would reopen deterrent decisions, alliance architectures premised on extended deterrence would renegotiate, the export-control cartel would lose its legal anchor, and regional arms-race spirals would resume in the Middle East, East Asia, and South Asia — the arrangements of dozens of states depend on it.
% FOUNDING_PROBLEM: In the 1960s the forecast was a proliferation cascade — official estimates ran to twenty-five or more nuclear states within two decades — driven by security-dilemma dynamics in every region. The bargain built to stop it: cap the club at the existing five, buy everyone else off with peaceful-technology access and a good-faith disarmament commitment by the five.
% FOUNDING_PROBLEM_CORROBORATION: The horizontal half's liveness is attested from outside the beneficiary set by IAEA reporting on the DPRK and Iran files and by the 1998 South Asian tests. The vertical half's death is attested from outside the beneficiary set by the 122-state TPNW vote, SIPRI and independent arsenal-modernization data showing all five weapon states upgrading forces, and the Shultz-Perry-Kissinger-Nunn sequence of abolition appeals by former senior officials of the leading weapon state. No corroborating source outside the P5 attests that Article VI is being honored on any schedule; the P5 themselves attest only the horizontal half.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.64: high, but short of pure-extraction levels because cascade prevention is a real collective good and Article IV delivers tangible technology access. Suppression is authored at 0.66 as a raw structural property (unscaled by power or scope — scaling happens only to extractiveness in the engine): the regime's persistence depends on actively punishing exit (the North Korea precedent), conditioning cooperation on compliance, and ratcheting verification (Additional Protocol 1997, UNSCR 1540 2004). Theater ratio 0.38: the safeguards function is real, but a growing share of regime activity is ritual — review-conference consensus documents that bind nothing, Article VI progress reporting that reports stagnation. Accessibility_collapse 0.48: alternatives demonstrably persist (India, Pakistan, Israel stayed out and armed; North Korea exited; the TPNW route exists), so the regime does not collapse alternatives the way a natural limit would. Resistance 0.58: sustained Non-Aligned Movement bloc pressure, the 122-vote TPNW adoption, and Iranian procedural warfare are real, organized, and recurrently effective at blocking P5 preferences. The measurement series run on one shared grid (T=0,10,20,30,40,50 ≈ 1970-2020) with all three metrics authored at every point. All three rise: extraction accumulates as Article VI recedes (plateau during early post-Cold-War optimism, then acceleration after the 1995 indefinite extension removed the renegotiation lever), theater grows as review cycles ritualize, and the suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up — the machinery matured and hardened over the interval rather than staying static.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience four different constraints under the same text. From the P5 seat the regime is order-preserving stewardship they fund diplomatically and profit from structurally — effective extraction computes near zero or negative. From the safeguarded-majority seat it is enforced second-class citizenship: permanent renunciation paired with unfulfilled reciprocation. From the threshold-state seat it is the denial of a capability they paid to develop, compensated by an umbrella whose credibility they cannot verify. From the IAEA seat it is mission growth and professional purpose. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the P5, umbrella allies, and Article IV entrants toward the low-d (subsidized) pole; the P5's arbitrage-grade exit — they reinterpret the text for themselves while enforcing it on others — places them nearest the beneficiary end. Victim declarations drive the safeguarded majority and threshold states toward the high-d (full-target) pole; their constrained exit (sanctions exposure, alliance dependence) traps them near it. One override is declared: threshold_breakout_states carry a dual position (payer with secondary beneficiary status via extended deterrence), which a naive derivation would blur toward mid-scale; under this reading the deterrent denial dominates the compensation, so d is overridden to 0.70 at the powerful power atom. The TPNW coalition derives high d from its payer role despite its considerable agency — organized resistance under constrained exit is still targeted position. The nonparty armed states sit outside the derivation entirely: they refused the arrangement, so they are neither its beneficiaries nor its victims; they are the excluded seat whose absence at founding shaped the whole structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a predicted proliferation cascade of twenty-plus nuclear states — remains live, so no mandatrophy resolution is declared; the arrangement has not outlived its horizontal function. But the arrangement's second pillar has atrophied into performance, and the theater_ratio series (0.15 to 0.38) tracks that atrophy directly. The tangled-rope claim prevents mislabeling in both directions: a pure-extraction verdict would erase why roughly 180 states stay (genuine cascade fear, real Article IV goods, verifiable neighbor restraint), while a pure-coordination verdict would erase the enforcement asymmetry the measurement series documents — burdens universalized, benefits concentrated, reciprocation deferred indefinitely. The engine's per-seat computation decides whether the payer seats experience the arrangement as hybrid or as pure extraction; the corpus exists to take that measurement, not to presume it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_indexicality,
    'This constraint instantiates the oligopoly_enforcement_reading of kernel npt_treaty_1970; how would instantiating the sibling readings change the constraint''s structure and classification?',
    'Author the sibling stories (reciprocal_disarmament_reading, withdrawal_sovereignty_reading) against the same referent and compare computed classifications. The disagreement is located in three structural elements: the legal force of Article VI, the conditionality of Articles I-II obligations, and the standing of the Article X exit.',
    'Under the reciprocal reading, Article VI enters the binding-obligation set, NWS arsenals become part of the extraction surface, and the arrangement computes as pure extraction from the disarmament-claimant seats; under the withdrawal reading, enforcement universality dissolves and the sanctions machinery itself becomes the contested object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of the NPT kernel; epsilon is indexed to this reading over the fixed referent of the standing regime.').

omega_variable(
    verification_asymmetry_necessity,
    'Is the inspection-burden asymmetry (intrusive verification of non-weapon states, no audit jurisdiction over recognized weapon states'' military programs) an inherent property of arms-control verification or a constructed privilege?',
    'Technical assessment of universal-coverage proposals — fissile-material cutoff with verified production bans, managed-access warhead verification — against the declared-materials safeguards standard.',
    'If inherent, a slice of the measured burden is irreducible coordination cost and the tangled-rope structure strengthens; if constructed, the asymmetry is rent and the arrangement trends toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_necessity, empirical, 'Whether the safeguards asymmetry is technically forced or institutionally chosen.').

omega_variable(
    nnws_restraint_mechanism,
    'Is non-weapon-state restraint under the regime structural (alliance dependence, sanctions exposure) or internalized (absorbed nonproliferation norm)?',
    'Comparative trajectory analysis of states whose structural supports moved — Ukraine after the 1994 Budapest assurances, Belarus and Kazakhstan denuclearization, South Korean alliance fluctuations — versus states retaining guarantees.',
    'If internalized, restraint outlives enforcement decay and the regime''s suppressive requirement falls without behavioral change; if structural, enforcement erosion converts directly into breakout pressure and effective suppression rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_restraint_mechanism, empirical, 'Structural versus internalized sources of nonproliferation compliance.').

omega_variable(
    indefinite_extension_lever_loss,
    'Did the 1995 indefinite extension convert a renewable 25-year bargain into a permanent hierarchy by removing the renegotiation lever, and is the resulting arrangement now maintained by inertia rather than consent?',
    'Compare review-conference bargaining intensity and Article VI deliverables before and after 1995; test whether any disarmament benchmark became enforceable once expiry pressure vanished.',
    'If the lever''s removal drove the post-1995 rise visible in the measurement series, the arrangement''s steady state sits closer to inertial persistence than to a living bargain, and long-run classification drifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_lever_loss, conceptual, 'Whether permanence without benchmarks entrenched the two-tier structure.').

omega_variable(
    article_iv_offset_weight,
    'How much of the non-weapon-state burden is genuinely offset by Article IV technology transfer and the security value of neighbors'' restraint?',
    'Economic quantification of transferred reactor, fuel-assurance, and safety-training value against safeguard-compliance costs, combined with survey evidence on stated accession motives.',
    'Large offsets push the payer seats toward a coordinated-benefit experience; negligible offsets leave them near the full-target pole and strengthen the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_iv_offset_weight, empirical, 'Weight of the compensatory half of the grand bargain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(npt__tr_t10, observed).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(npt__tr_t20, observed).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(npt__tr_t30, observed).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(npt__tr_t40, observed).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(npt__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t10, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(npt__be_t10, observed).
narrative_ontology:measurement(npt__be_t20, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(npt__be_t20, observed).
narrative_ontology:measurement(npt__be_t30, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(npt__be_t30, observed).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement_basis(npt__be_t40, observed).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(npt__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t10, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(npt__su_t10, observed).
narrative_ontology:measurement(npt__su_t20, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(npt__su_t20, observed).
narrative_ontology:measurement(npt__su_t30, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(npt__su_t30, observed).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(npt__su_t40, observed).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(npt__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, tpnw_2017_prohibition_instrument).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_additional_protocol_regime).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the NPT bargain': the kernel npt_treaty_1970 splits into three readings with distinct epsilon values and victim sets — this oligopoly_enforcement_reading (epsilon 0.64; victims: safeguarded NNWS and threshold states), reciprocal_disarmament_reading (pulls NWS arsenals onto the extraction surface; epsilon higher from the disarmament-claimant seat), and withdrawal_sovereignty_reading (makes the enforcement machinery itself the contested object). Causal structure runs upstream-to-downstream: this reading's operation feeds legitimacy pressure into both siblings — each unfulfilled Article VI cycle strengthens the reciprocal reading, and the DPRK exit precedent strengthens the sovereignty reading. The TPNW instrument and the Additional Protocol regime are downstream institutional artifacts linked for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
