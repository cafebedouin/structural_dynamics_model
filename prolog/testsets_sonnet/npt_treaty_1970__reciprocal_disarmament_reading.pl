% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: NPT Article VI as Binding Reciprocal Disarmament Bargain
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the reciprocal-disarmament reading of the NPT
 *   kernel: Article VI's 'undertake to pursue negotiations in good faith' is
 *   treated as a binding legal obligation with temporal weight, coequal with
 *   — and the structural counterpart to — the horizontal nonproliferation
 *   obligations in Articles I and II. Under this reading, the treaty is a
 *   genuine bargain: NNWS permanent renunciation in exchange for NWS progress
 *   toward elimination. The absence of any Article VI verification mechanism,
 *   on this reading, is not an implementation gap but a structural injustice
 *   — the treaty's enforcement architecture was built asymmetrically from the
 *   outset, verifying one side's compliance in exhaustive technical detail
 *   while treating the other side's obligation as self-policing. Over five
 *   decades, NWS arsenal modernization (new delivery platforms, warhead
 *   life-extension programs, expanded nuclear doctrine scope) is read, under
 *   this framing, as an accumulating record of non-fulfillment rather than
 *   sovereign discretion.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary — administers the review process, controls modernization decisions, faces no verification
 *   - non_nuclear_weapon_states_coalition: payer — bears full safeguards burden, watches reciprocal obligation go unmet
 *   - nws_strategic_autonomy: payer (non-agent structural condition) — modernization freedom becomes contested legal terrain under this reading
 *   - disarmament_civil_society: excluded — advocacy voice without seat in authoritative interpretation
 *   - iaea_and_review_conference_secretariat: observer — administers only the horizontal half of the bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Binding Reciprocal Disarmament Bargain").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '3d12e2e2-437e-4e0c-937a-adca8cd76eb8').
narrative_ontology:cs_kernel_codification('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', fixed_text).
narrative_ontology:cs_authority_grounding('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', distributed).
narrative_ontology:cs_reading_relation('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', foundational, article_vi_coequal_binding_obligation).
narrative_ontology:cs_axiom_status(article_vi_coequal_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', article_vi_coequal_binding_obligation, conventional).
narrative_ontology:cs_axiom('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', secondary, temporal_urgency_of_disarmament_duty).
narrative_ontology:cs_axiom_status(temporal_urgency_of_disarmament_duty, holdable).
narrative_ontology:cs_axiom_grounding('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', temporal_urgency_of_disarmament_duty, instrumental).
narrative_ontology:cs_reference_frame('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', reciprocal_grand_bargain_1968).
narrative_ontology:cs_drift_state('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', post_cold_war_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d12e2e2-437e-4e0c-937a-adca8cd76eb8', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_strategic_stability_regime).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_civil_society).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, reciprocal_bargain_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, temporal_urgency_of_article_vi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear powers accepted Article VI's disarmament language as the price of universalizing the horizontal nonproliferation regime. In practice they administer the review-conference process, control what counts as 'good faith negotiation,' and have continued to modernize arsenals (delivery systems, warhead life-extension, new platforms) while treating the vertical obligation as open-ended. They face no independent verification mechanism for Article VI compliance and no penalty structure comparable to the safeguards regime that binds non-nuclear states.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% States that renounced nuclear weapons acquisition in exchange for a legal promise of good-faith disarmament negotiations and eventual elimination. They bear the full weight of IAEA safeguards, export controls, and technology-denial regimes while watching the reciprocal obligation go substantially unfulfilled across five decades. Exit via withdrawal (Article X) carries severe diplomatic and security costs, and re-entry into weapons development would trigger sanctions regimes the NWS themselves helped construct.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, payer,
    organized, generational, trapped, global).

% Under the reciprocal-bargain reading, the NWS's freedom to modernize, deploy, and doctrinally rely on nuclear arsenals is treated as a constrained good rather than a sovereign prerogative — every modernization program becomes a data point in an accumulating breach record. This is not an actor but a structural condition: the reading itself narrows what counts as legitimate NWS behavior, converting strategic modernization decisions into contested legal events.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy).

% ICAN, humanitarian disarmament coalitions, and non-nuclear middle powers (the Humanitarian Initiative, the TPNW drafters) argue Article VI's 'undertake to pursue' language creates a binding obligation with a good-faith timeline that has been breached. They are not seated in the NPT's formal review process as voting parties and their legal arguments are treated as advocacy rather than authoritative treaty interpretation by the NWS-dominated review conferences.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_civil_society, excluded,
    moderate, generational, constrained, global).

% The broader nonproliferation architecture (IAEA safeguards, export control regimes, nuclear-weapon-free zones) depends on the NPT's near-universal membership, which in turn depends on the reciprocal-bargain narrative retaining enough credibility to keep non-nuclear states inside the treaty rather than pursuing indigenous deterrents.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, global_strategic_stability_regime, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, global_strategic_stability_regime).

% Administers safeguards verification for horizontal compliance but has no comparable mandate or technical mechanism to verify Article VI vertical disarmament progress; documents NWS arsenal data as reported voluntarily rather than through inspection, producing an asymmetric evidentiary record between the two obligations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_and_review_conference_secretariat, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real collective-action problem: without a credible bargain, near-universal renunciation of nuclear weapons acquisition would not have been achievable, since no state would disarm-by-abstention while others retained the option. The reciprocal structure — 'you don't get them, we give them up eventually' — is what made near-universal membership possible.
% TRANSFER_FUNCTION: Moves security-relevant legal restraint from non-nuclear-weapon states (permanent renunciation, enforced by safeguards) to nuclear-weapon states (a time-unbound promissory obligation with no verification mechanism), while the concrete costs of compliance (inspections, export controls, foreclosed weapons programs) run overwhelmingly one direction.
% ABSENT_VOICES: Humanitarian disarmament coalitions, the TPNW signatory states, and non-nuclear middle powers who argue Article VI creates enforceable temporal obligations are not seated as authoritative interpreters in the NWS-dominated review conference process; their legal position (endorsed by the ICJ's 1996 advisory opinion language on good-faith pursuit) is treated as aspirational commentary rather than binding interpretation.
% DISAPPEARANCE_RATIONALE: If the reciprocal-bargain reading collapsed entirely — if Article VI were formally read as non-binding aspiration rather than a legal undertaking with temporal weight — the NNWS coalition's central legal and moral leverage for demanding disarmament progress would vanish, the TPNW movement would lose its principal doctrinal anchor point, and the NPT's legitimacy claim to universality would be exposed as resting solely on horizontal nonproliferation enforcement rather than a genuine bargain; several NNWS blocs have signaled this would accelerate withdrawal consideration or regional deterrent hedging.
% FOUNDING_PROBLEM: In 1968, negotiators needed near-universal renunciation of nuclear weapons acquisition to prevent runaway proliferation, but no non-nuclear state would accept permanent, verified renunciation without a reciprocal legal commitment from existing nuclear powers to eventually disarm — otherwise the treaty simply froze in place a permanent two-tier hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice's 1996 Advisory Opinion on the Legality of the Threat or Use of Nuclear Weapons stated unanimously that Article VI entails an obligation to pursue negotiations in good faith to a conclusion — a corroborating source external to both the NWS and the NNWS coalition. Successive NPT Review Conference final documents (2000, 2010) issued consensus statements affirming disarmament commitments, though these were negotiated with NWS participation and are not independent of the benefiting parties; the ICJ opinion is the clearest external corroboration that the obligation is legally live rather than aspirational.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 by 2025, reflecting a widening gap between the enforced horizontal obligation and the substantially unenforced vertical one — the NNWS coalition pays a real, verified, ongoing cost while the NWS's reciprocal commitment carries no comparable enforcement teeth. Theater ratio rises sharply (0.20 to 0.62) because an increasing share of NWS engagement with Article VI takes the form of review-conference rhetorical commitment (consensus final documents, disarmament 'pillars' language) without corresponding verified arsenal reduction — indeed alongside modernization. Suppression is moderate-high (0.58) and reflects the diplomatic and structural costs of NNWS exit (Article X withdrawal triggers severe consequences) rather than direct coercion. Accessibility collapse is moderate (0.5): NNWS states retain the formal option to withdraw or pursue TPNW-style parallel regimes, so alternatives have not collapsed completely, but the practical costs of exit are severe enough to constrain most behavior to within-treaty advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the arrangement is a working coordination structure they steward in good faith, subject to legitimate incremental progress on a long-horizon civilizational problem. From the NNWS coalition seat, the same structure is an enforced asymmetric bargain: their compliance is inspected line-by-line while the reciprocal promise has no comparable mechanism. The engine computes these divergently from the structural data — the NWS's institutional power, arbitrage exit options, and beneficiary role versus the NNWS coalition's organized-but-trapped position under a payer role with no verification recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS sit at the beneficiary end: they retain full sovereign discretion over their arsenals, administer the interpretive process, and bear no verification burden — the constraint subsidizes their position. The NNWS coalition sits at the target end: permanent, verified renunciation with only a promissory, unverified reciprocal claim in return — trapped exit options given the diplomatic and security costs of withdrawal. NWS strategic autonomy is declared as a non-agent structural victim: under this reading, the modernization decisions the NWS make are not personally attributable extraction but a structural condition — the reading itself narrows the space of unconstrained NWS action, converting doctrine and procurement choices into contested compliance data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universalizing non-proliferation via credible reciprocal bargain) remains partially live for the coordination function — near-universal NPT membership persists and horizontal proliferation has been substantially contained — but the vertical disarmament half of the bargain is contested as dead-in-practice while formally still asserted as live by all review-conference final documents. This is precisely the tangled-rope signature: coordination benefit (universality, stability) is real and ongoing, but it rides alongside asymmetric extraction (NNWS bears verified costs; NWS's reciprocal costs remain unverified and arguably undelivered). Classifying this as pure snare would erase the genuine coordination value the regime has produced (the horizontal nonproliferation success is real); classifying it as pure rope would erase the accumulating record of unequal burden-bearing that the NNWS coalition and ICJ language document. Tangled rope holds both facts simultaneously without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_ambiguity,
    'Does Article VI''s ''undertake to pursue negotiations in good faith on effective measures relating to cessation... and to nuclear disarmament'' create a binding obligation with an enforceable timeline, or is it aspirational language whose only binding content is the duty to negotiate (not to conclude or achieve any particular outcome)?',
    'This is fundamentally a question of treaty interpretation under the Vienna Convention on the Law of Treaties (ordinary meaning, context, object and purpose, subsequent practice). The 1996 ICJ Advisory Opinion found a binding obligation to pursue negotiations to a conclusion in good faith, but this is advisory, not adjudicative, and NWS practice has not treated it as creating enforceable deadlines. No binding international tribunal has issued a contentious-case ruling on Article VI compliance.',
    'If Article VI is binding with temporal urgency (this reading), five decades of arsenal modernization constitute an accumulating breach record and the tangled_rope classification with NWS strategic autonomy as victim is structurally correct. If Article VI is genuinely non-binding aspiration (the oligopoly_enforcement_reading), the same NWS conduct is lawful discretion and the constraint collapses toward a rope or even mountain-adjacent reading of horizontal nonproliferation alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_status_ambiguity, conceptual, 'Whether Article VI creates a binding, time-bound legal obligation or aspirational language — the central interpretive fork of the NPT kernel.').

omega_variable(
    committer_frame_reading_disagreement_location,
    'This story is one reading (reciprocal_disarmament_reading) of the npt_treaty_1970 kernel; sibling readings (oligopoly_enforcement_reading, withdrawal_sovereignty_reading) locate the treaty''s core legal content differently. Where exactly does the disagreement sit — is it about which article is primary, or about the nature of legal obligation created by ''undertake to pursue'' language itself?',
    'The disagreement is located specifically at (a) whether Article VI creates parity with Articles I-II as coequal binding obligations versus a hierarchically subordinate/aspirational status, and (b) whether the absence of a verification mechanism for Article VI is itself evidence the drafters intended it as non-binding (oligopoly_enforcement_reading''s implicit argument) or as an oversight/injustice to be remedied (this reading''s argument). A sibling reading adopting the oligopoly_enforcement_reading would not treat NWS modernization as contributing to any victim-class formation at all — nws_strategic_autonomy would not appear as a victim, and the NNWS coalition''s leverage claims would be read as political advocacy rather than legal entitlement.',
    'Under this reading, NWS strategic autonomy enters the victim set and the constraint classifies as tangled_rope with real extraction. Under oligopoly_enforcement_reading, the same facts describe a rope (or near-mountain) constraint with no comparable victim class on the vertical side, since there is no binding vertical obligation to breach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_disagreement_location, conceptual, 'Documents where the kernel-level disagreement is structurally located: primacy-of-article versus nature-of-obligation.').

omega_variable(
    withdrawal_sovereignty_interaction,
    'If NNWS exit (Article X withdrawal) is read as a legitimate, low-cost exercise of sovereignty contingent on the security environment (the withdrawal_sovereignty_reading), does that undercut this reading''s characterization of NNWS exit_options as ''trapped''?',
    'Empirical examination of actual withdrawal cases (DPRK 2003) and the diplomatic/security consequences that followed — sanctions regimes, security council referrals, alliance realignment — versus the formal legal cost of withdrawal under Article X''s own terms (three months'' notice, no Security Council approval required).',
    'If withdrawal is genuinely low-cost and sovereignty-preserving, NNWS exit_options should be coded closer to ''constrained'' or even ''mobile'' rather than ''trapped,'' which would reduce this reading''s suppression score and shift the classification away from tangled_rope toward something closer to a contested-but-voluntary coordination rope. If withdrawal is high-cost in practice (as the DPRK case and subsequent sanctions history suggest), ''trapped'' is the accurate coding and this reading''s extraction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_sovereignty_interaction, empirical, 'Whether the sibling withdrawal_sovereignty_reading''s characterization of exit as low-cost sovereignty is empirically supported, which would weaken this reading''s trapped-exit coding for the NNWS coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.58).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the npt_treaty_1970 kernel, decomposed per the ε-invariance principle: measuring 'the NPT' through the lens of Article VI's binding status yields a substantially different ε (0.68, tangled_rope with NWS as beneficiary/victim structure inverted relative to horizontal readings) than measuring it through the lens of Article I-II primacy (oligopoly_enforcement_reading, where Article VI is contingent and the constraint reads closer to a stable coordination mechanism with NNWS as the primarily-served party) or through the lens of Article X withdrawal legitimacy (withdrawal_sovereignty_reading, which reframes NNWS 'trapped' exit as sovereign choice). Each reading is authored as its own ε-invariant constraint with its own stakeholder set and its own claimed_type; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
