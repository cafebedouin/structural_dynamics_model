% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI as Binding Disarmament Obligation (Reciprocal Bargain Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The Treaty on the Non-Proliferation of Nuclear Weapons (1968) is read
 *   here as codifying a reciprocal bargain: near-universal NNWS renunciation
 *   of nuclear acquisition in exchange for a legally binding NWS commitment,
 *   under Article VI, to pursue in good faith and conclude negotiations on
 *   nuclear disarmament. Under this reading, the temporal urgency is real —
 *   'undertakes to pursue' is treaty language creating an obligation of
 *   conduct with an implicit expectation of progress, not a permanently
 *   deferrable aspiration. Fifty-plus years of NWS arsenal retention,
 *   modernization programs (US B61-12, Russian Sarmat, Chinese arsenal
 *   expansion, UK/French modernization), and the absence of any verification
 *   mechanism for Article VI compliance constitute, on this reading, an
 *   ongoing structural breach — not an implementation detail to be worked out
 *   at future review conferences, but the persistence of an extraction that
 *   the horizontal nonproliferation regime's legitimacy depends on resolving.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.71).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Binding Disarmament Obligation (Reciprocal Bargain Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'c9394f0b-65ed-4231-9b51-4589e4a34506').
narrative_ontology:cs_kernel_codification('c9394f0b-65ed-4231-9b51-4589e4a34506', fixed_text).
narrative_ontology:cs_authority_grounding('c9394f0b-65ed-4231-9b51-4589e4a34506', distributed).
narrative_ontology:cs_reading_relation('c9394f0b-65ed-4231-9b51-4589e4a34506', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9394f0b-65ed-4231-9b51-4589e4a34506', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('c9394f0b-65ed-4231-9b51-4589e4a34506', foundational, article_vi_creates_binding_obligation_of_conduct).
narrative_ontology:cs_axiom_status(article_vi_creates_binding_obligation_of_conduct, holdable).
narrative_ontology:cs_axiom_grounding('c9394f0b-65ed-4231-9b51-4589e4a34506', article_vi_creates_binding_obligation_of_conduct, conventional).
narrative_ontology:cs_axiom('c9394f0b-65ed-4231-9b51-4589e4a34506', foundational, horizontal_and_vertical_nonproliferation_are_single_indivisible_bargain).
narrative_ontology:cs_axiom_status(horizontal_and_vertical_nonproliferation_are_single_indivisible_bargain, holdable).
narrative_ontology:cs_axiom_grounding('c9394f0b-65ed-4231-9b51-4589e4a34506', horizontal_and_vertical_nonproliferation_are_single_indivisible_bargain, instrumental).
narrative_ontology:cs_reference_frame('c9394f0b-65ed-4231-9b51-4589e4a34506', reciprocal_bargain_at_signature_1968).
narrative_ontology:cs_drift_state('c9394f0b-65ed-4231-9b51-4589e4a34506', post_2017_tpnw_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9394f0b-65ed-4231-9b51-4589e4a34506', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_security_architecture).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, reciprocal_bargain_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, temporal_urgency_of_disarmament).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear powers (US, Russia, UK, France, China) accepted Article VI's disarmament language in 1968 in exchange for near-universal acceptance of their weapons monopoly under Articles I-II. Fifty-plus years later they retain full arsenals, conduct modernization programs, and treat Article VI as a direction of travel rather than an enforceable deadline. They administer the review conference process and can shape what counts as compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% Under this reading, the same states that benefit from the horizontal-nonproliferation bargain are also bound by a genuine, time-urgent legal obligation to negotiate disarmament in good faith. Their freedom to modernize arsenals indefinitely, extend deterrence postures, and treat weapons as permanent strategic assets is constrained by a standing legal commitment they signed and have not fulfilled — the obligation itself is a cost this reading imposes on their room to maneuver, distinguishing it from readings that treat Article VI as aspirational.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy, payer,
    institutional, generational, constrained, global).

% The roughly 185 non-nuclear signatories forwent the sovereign option of acquiring nuclear weapons in exchange for the NWS's Article VI commitment and access to peaceful nuclear technology under Article IV. They bear the ongoing cost of an unfulfilled bargain: they gave up a real capability and received, in return, a promise with no verification mechanism, no timeline enforcement, and no penalty for non-performance. Their formal exit (Article X withdrawal) is available but reputationally and strategically catastrophic, so most remain locked into the bargain while gaining normative leverage at review conferences and in fora like the Humanitarian Initiative and TPNW.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary).

% Administers safeguards and verification for Articles I-III but has no comparable verification mandate for Article VI disarmament progress. Review conferences produce consensus documents (or, increasingly, no consensus) that document NWS disarmament rhetoric without a binding audit mechanism, structurally unable to compel the obligation this reading insists is legally binding.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_and_review_conference_apparatus, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, iaea_and_review_conference_apparatus, observer).

% A coalition of NNWS and civil society actors concluded the NPT's Article VI bargain has structurally failed and built the Treaty on the Prohibition of Nuclear Weapons (2017) as a parallel legal instrument. They are not parties to NPT review conference decision-making in any binding sense and are frequently characterized by NWS as undermining the NPT rather than vindicating its unmet promise; their objection — that fifty years of 'good faith negotiation' language has produced no disarmament — is central to this reading but structurally sidelined in NPT institutional processes.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_ban_treaty_states, excluded,
    organized, generational, mobile, global).

% The ICJ's 1996 Advisory Opinion held unanimously that Article VI creates an obligation to pursue in good faith and bring to a conclusion negotiations leading to nuclear disarmament — language this reading treats as authoritative confirmation that the obligation is legally binding, not merely hortatory, and that its indefinite non-fulfillment constitutes a breach rather than a permissible delay.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_court_of_justice_1996_opinion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article VI, on this reading, coordinates a genuine intertemporal bargain: NNWS states permanently forgo acquiring nuclear weapons and accept intrusive safeguards, in exchange for a legally binding NWS commitment to pursue negotiations toward eventual disarmament. The coordination problem solved is preventing a multipolar nuclear arms race by trading present restraint for a credible future disarmament trajectory.
% TRANSFER_FUNCTION: The arrangement moves strategic security guarantees and normative legitimacy from NNWS states to NWS states (via near-universal renunciation of weapons acquisition and acceptance of the nonproliferation order) in exchange for a promise of reciprocal restraint on the NWS side — a promise this reading holds is legally enforceable and whose non-performance constitutes an ongoing uncompensated transfer from NNWS to NWS.
% ABSENT_VOICES: TPNW states and disarmament-focused civil society organizations argue the bargain has failed as a matter of fact and law but have no binding vote in NPT review conference outcomes; their alternative instrument (the Ban Treaty) is treated by NWS as extraneous rather than as evidence of Article VI's non-fulfillment.
% DISAPPEARANCE_RATIONALE: If the Article VI obligation and the surrounding review-conference apparatus vanished overnight, the normative leverage NNWS states currently use to pressure NWS modernization and arms control negotiations would disappear; some analysts believe several threshold states would reconsider nuclear acquisition once the reciprocal bargain's legal claim was gone, since the renunciation was conditioned on a disarmament trajectory that would no longer even be nominally binding.
% FOUNDING_PROBLEM: In 1968, the founding problem was twofold: prevent a cascade of new nuclear weapon states (horizontal proliferation) while giving non-nuclear states a legally cognizable reason — beyond mere power asymmetry — to accept permanent non-acquisition: a binding commitment that the five recognized powers would not simply lock in permanent nuclear apartheid but would move toward eliminating their own arsenals.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ's 1996 Advisory Opinion, issued by a body outside both the NWS and NNWS camps, corroborates that the obligation is legally live and unfulfilled. Independent arms control scholarship (SIPRI, the Bulletin of the Atomic Scientists) and the TPNW's founding documents — produced by NNWS coalitions rather than NWS beneficiaries — corroborate the founding-problem-is-unresolved reading. NWS governments themselves largely characterize the founding problem as substantially addressed through arms control agreements (New START, historical stockpile reductions), a claim this reading treats as self-serving given ongoing modernization programs.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71 by 2024) reflects the widening gap between the bargain's terms and its performance: NNWS states have fully performed their side (near-universal non-acquisition, safeguards acceptance) while NWS performance has, on this reading, stalled or reversed via modernization. Theater ratio (0.58) captures the review-conference apparatus's increasing tendency to produce disarmament rhetoric, working groups, and consensus-document language without binding mechanisms — a rising proxy-for-substance pattern over the interval. Suppression (0.62) reflects the structural cost of Article X withdrawal (reputational catastrophe, strategic isolation) that keeps NNWS states locked into a bargain even as its NWS-side performance is disputed; this is a raw structural property and is not scaled by the engine, whereas extractiveness IS scaled by scope (global) and directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS sit as both agenda_setter (they administer review conferences and safeguards enforcement asymmetrically) and beneficiary under the horizontal-nonproliferation half of the bargain, but under THIS reading they are simultaneously a payer seat: nws_strategic_autonomy is listed as a victim group because the reading imposes on them a genuine, binding cost — constrained freedom to treat their arsenals as permanent assets. This is the reading's key structural delta from the oligopoly_enforcement_reading, where NWS autonomy would not appear as constrained at all. NNWS coalition states are payers (they gave up a sovereign option and received an unperformed promise) but also nominal beneficiaries (extended deterrence umbrellas, peaceful nuclear technology access under Article IV) — hence the secondary_role. Their d sits nearer the target end because their exit option (Article X withdrawal) is technically available but effectively foreclosed by cost, which the engine's directionality derivation should treat as constrained rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors. First, it avoids treating the NPT as a pure Rope (mutual benefit, minimal coercion) by insisting the coordination function (preventing horizontal proliferation) is real but co-exists with an asymmetric extraction (NWS retaining strategic autonomy while NNWS bears the sunk cost of renunciation) that the treaty's own enforcement apparatus (IAEA safeguards, export control regimes) actively maintains. Second, it avoids treating the arrangement as a pure Snare by preserving the genuine coordination value — the treaty has, on most accounts, meaningfully slowed horizontal proliferation — which is why tangled_rope, not snare, is the structurally correct claim. The founding problem's contested status (dead for NWS, live for NNWS coalition and the ICJ) is precisely the R5 corroboration mismatch this reading exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI a legally binding obligation of conduct with temporal urgency (as the ICJ''s 1996 Advisory Opinion and this reading hold), or a contingent, aspirational commitment subordinate to Articles I-II (as the oligopoly_enforcement_reading holds)?',
    'State practice analysis of whether NWS treat Article VI compliance as legally consequential (e.g., in ICJ proceedings, UN Security Council referrals, or treaty-based sanctions) versus purely diplomatic/rhetorical; formal legal scholarship on treaty interpretation under VCLT Article 31 applied to ''undertakes to pursue in good faith.''',
    'If Article VI is binding with temporal urgency, NWS non-performance constitutes an ongoing structural breach (this reading''s ε ≈ 0.71 for NWS conduct). If Article VI is aspirational, the same non-performance is not extractive at all, and ε for the identical historical facts drops toward the oligopoly_enforcement_reading''s much lower value — this IS the ε-invariance boundary between the two sibling constraints, not a parameter internal to either.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Whether Article VI''s legal status is binding-with-urgency or aspirational — the core premise dividing this reading from the oligopoly_enforcement_reading.').

omega_variable(
    verification_gap_as_injustice_vs_technicality,
    'Is the absence of any Article VI verification mechanism (unlike the robust IAEA safeguards regime for Articles I-III) a structural injustice built into the bargain''s design, or merely an unresolved implementation detail that review conferences are still working out?',
    'Comparative institutional analysis: examine whether the asymmetry in verification investment (safeguards budget/legal infrastructure for horizontal nonproliferation vs. near-zero comparable infrastructure for vertical disarmament) reflects a design choice traceable to NWS negotiating leverage in 1968, versus a genuinely harder technical verification problem (verifying disarmament trajectories is arguably more technically difficult than verifying non-acquisition).',
    'If the gap is structural injustice, it corroborates this reading''s claim that NWS strategic autonomy is a victim-imposing constraint they evade rather than merely an unfinished project — supporting tangled_rope classification. If it is a genuine technical/implementation gap, the reading''s extraction claim weakens and the constraint looks more like an incompletely specified rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_gap_as_injustice_vs_technicality, conceptual, 'Whether the Article VI verification gap is designed structural injustice or unresolved implementation difficulty.').

omega_variable(
    kernel_reading_selection_evidence,
    'This story treats the reciprocal_disarmament_reading as the correct lens; what evidence or context most directly supports selecting this reading over the sibling oligopoly_enforcement_reading and withdrawal_sovereignty_reading for a given analytical purpose?',
    'Documented via the ICJ''s 1996 Advisory Opinion (external judicial authority favoring the binding reading), NNWS state practice at review conferences (consistently invoking Article VI as binding), and the TPNW''s founding rationale (explicitly citing NPT Article VI non-fulfillment) as convergent evidence for this reading''s institutional traction, while noting NWS state practice and doctrine (treating Article VI as contingent on the broader security environment) as convergent evidence for the sibling readings.',
    'Selecting this reading changes the constraint''s classification from a likely rope/mountain-adjacent reading (oligopoly_enforcement) or a sovereignty-neutral reading (withdrawal_sovereignty) to tangled_rope with NWS autonomy as a named victim group — a materially different structural claim about the same treaty text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Documents the framing choice among three coherent kernel readings and the signals guiding selection of the reciprocal_disarmament_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_ban_treaty_2017).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_treaty_1970 kernel, each authored as a separate, ε-invariant story per the decomposition principle. The oligopoly_enforcement_reading treats Article VI as contingent and would author a substantially lower ε for NWS conduct under the same historical facts. The withdrawal_sovereignty_reading treats all obligations, including Article VI, as contingent on the security environment via Article X, which would reframe NWS non-performance as legitimate reassessment rather than breach. This reading's higher ε and tangled_rope classification are specific to treating Article VI as binding-with-urgency; do not average across readings or treat their divergent ε values as measurement noise on a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
