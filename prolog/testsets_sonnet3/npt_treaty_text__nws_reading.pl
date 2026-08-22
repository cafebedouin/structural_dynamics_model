% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Non-Proliferation Obligation Read as Binding on NNWS, Disarmament as Aspirational (NWS Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the NWS reading of the NPT kernel: Article VI's
 *   'undertake to pursue negotiations in good faith... at an early date on
 *   effective measures relating to cessation of the nuclear arms race and to
 *   nuclear disarmament' is treated by nuclear-weapon states and their allied
 *   interpretive apparatus as a hortatory, non-justiciable aspiration, while
 *   Article II/III non-proliferation obligations on NNWS are treated as
 *   immediately binding and subject to intrusive, resourced verification. The
 *   asymmetry is not incidental to this reading — it IS this reading's
 *   structural content. The sibling nnws_reading (disarmament as binding
 *   Article VI obligation, non-proliferation as conditional restraint) and
 *   withdrawal_threshold_reading (Article X threshold contest) are separate
 *   constraints with their own ε and stakeholder structures; this file does
 *   not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Non-Proliferation Obligation Read as Binding on NNWS, Disarmament as Aspirational (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '8814dd17-981a-44bd-86d1-0ad77dff6a63').
narrative_ontology:cs_kernel_codification('8814dd17-981a-44bd-86d1-0ad77dff6a63', fixed_text).
narrative_ontology:cs_authority_grounding('8814dd17-981a-44bd-86d1-0ad77dff6a63', practice).
narrative_ontology:cs_interpretation_layer_present('8814dd17-981a-44bd-86d1-0ad77dff6a63').
narrative_ontology:cs_reading_relation('8814dd17-981a-44bd-86d1-0ad77dff6a63', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('8814dd17-981a-44bd-86d1-0ad77dff6a63', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('8814dd17-981a-44bd-86d1-0ad77dff6a63', foundational, disarmament_obligation_non_justiciable).
narrative_ontology:cs_axiom_status(disarmament_obligation_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('8814dd17-981a-44bd-86d1-0ad77dff6a63', disarmament_obligation_non_justiciable, conventional).
narrative_ontology:cs_axiom('8814dd17-981a-44bd-86d1-0ad77dff6a63', secondary, strategic_stability_conditions_precedence).
narrative_ontology:cs_axiom_status(strategic_stability_conditions_precedence, holdable).
narrative_ontology:cs_axiom_grounding('8814dd17-981a-44bd-86d1-0ad77dff6a63', strategic_stability_conditions_precedence, instrumental).
narrative_ontology:cs_reference_frame('8814dd17-981a-44bd-86d1-0ad77dff6a63', id_1968_grand_bargain_framing).
narrative_ontology:cs_drift_state('8814dd17-981a-44bd-86d1-0ad77dff6a63', post_cold_war_indefinite_extension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8814dd17-981a-44bd-86d1-0ad77dff6a63', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_safeguards_apparatus).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, nnws_civilian_nuclear_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and modernize their arsenals while treating Article VI's 'negotiations in good faith... at an early date' as non-binding, non-justiciable aspiration. Control the interpretive apparatus (review conferences, P5 process statements) that determines what counts as compliance. Face no enforcement mechanism, no timeline, and no tribunal that can compel disarmament steps. Their exit from any disarmament obligation is essentially costless because the treaty text supplies no trigger or remedy.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary).

% Administers comprehensive safeguards agreements and additional protocols directed almost entirely at NNWS nuclear programs. Its budget, mandate growth, and institutional relevance derive from monitoring horizontal proliferation; it has no comparable mandate or resourcing to verify NWS disarmament steps, which structurally reinforces the asymmetry it operates within.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_safeguards_apparatus, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_safeguards_apparatus, agenda_setter).

% Accepted permanent non-acquisition of nuclear weapons and intrusive safeguards inspections in exchange for a disarmament promise that carries no timeline, no benchmark, and no enforcement. Withdrawal from the treaty is diplomatically and economically catastrophic (sanctions, isolation, loss of civilian nuclear cooperation), so exit is trapped in practice even though Article X exists on paper. Bear the full weight of verification intrusiveness while receiving none of the reciprocal verification against NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    moderate, generational, trapped, global).

% Civilian nuclear power and research programs in NNWS bear compliance costs, inspection burdens, and technology transfer restrictions (via the Nuclear Suppliers Group and Article III safeguards) that NWS domestic industries do not face in equivalent form. Cannot easily relocate operations outside the treaty regime without losing access to enrichment technology, reactor fuel markets, and international cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nnws_civilian_nuclear_industries, payer,
    moderate, biographical, constrained, national).

% Includes the Humanitarian Initiative states, ICAN, and non-signatory states pursuing the Treaty on the Prohibition of Nuclear Weapons. Argue Article VI creates a genuine, time-sensitive legal obligation, citing the 1996 ICJ Advisory Opinion's unanimous finding of a good-faith obligation to conclude negotiations. Have no seat in the NPT review conference consensus process that produces the operative interpretation, and their preferred reading is treated as aspirational rhetoric rather than binding law by the P5.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% The quinquennial review process where the interpretive contest over Article VI plays out through consensus-based final documents. Consensus requirements give NWS effective veto power over any outcome document language that would harden disarmament into enforceable obligation, which is why review conferences frequently end without a consensus document at all.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, npt_review_conference_process, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the spread of nuclear weapons to additional states, reducing the number of independent nuclear decision-makers and lowering the probability of nuclear war, miscalculation, or unauthorized use — a genuine collective security good that most parties, including most NNWS, value independent of the disarmament dispute.
% TRANSFER_FUNCTION: Moves permanent non-acquisition commitments, safeguards compliance costs, and technology-transfer restrictions from NNWS to the international regime, while NWS retain their arsenals and bear no reciprocal, verified, time-bound disarmament obligation. Verification resources and institutional attention flow toward monitoring NNWS and away from monitoring NWS.
% ABSENT_VOICES: States and coalitions pursuing the Treaty on the Prohibition of Nuclear Weapons (which none of the P5 have joined) argue explicitly against the aspirational reading; humanitarian-impact conference participants and the ICJ's 1996 advisory language are cited by NNWS advocates but carry no binding force inside the NPT's own consensus-based interpretive process, where P5 objection is functionally a veto.
% DISAPPEARANCE_RATIONALE: If the NWS reading of Article VI as non-binding aspiration were displaced by an enforceable-obligation reading, NWS would face structured negotiation timelines, verification requirements, and potential noncompliance consequences for arsenal retention — the current asymmetric bargain (NNWS restraint purchased against indefinite NWS retention) would need to be renegotiated or would collapse, likely accelerating NNWS movement toward instruments like the TPNW.
% FOUNDING_PROBLEM: In 1968, the treaty was built to freeze the nuclear club at five declared possessors and prevent horizontal spread, while offering NNWS a political trade: accept permanent non-acquisition in exchange for a good-faith commitment that the possessor states would move toward eliminating their own arsenals over time.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ's 1996 Advisory Opinion (an outside judicial body, not a treaty party) found unanimously that Article VI creates an obligation to pursue negotiations in good faith to a conclusion, contradicting the NWS reading that no binding obligation exists. Independent arms-control research bodies (SIPRI, the Arms Control Association) and successive NPT PrepCom working papers from non-P5 states document continued arsenal modernization by all nine possessor states as evidence the founding bargain's second half has not been honored; NWS governments themselves attest the obligation is aspirational and contingent on strategic conditions, which is a self-interested account from the benefiting parties, not outside corroboration of the aspirational reading.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.55 to 0.78) because the asymmetry compounds: NWS modernization programs (US, Russia, UK, France, China, plus de facto possessors) continue for five-plus decades while the 'early date' language accumulates decades of non-fulfillment without any interpretive consequence inside the NPT's own consensus process. Theater ratio climbs (0.20 to 0.45) as review conferences produce increasingly elaborate process language, working groups, and 'action plans' (notably 2010's 64-point action plan) that substitute for binding steps — performative diplomatic output replacing verified reduction. Suppression (0.62 at endpoint) reflects the trapped exit position of NNWS: Article X withdrawal is nominally available but carries such severe diplomatic and economic consequences that it functions as near-trapped, not truly constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, this is coordination: preventing further proliferation is a real collective good, and disarmament remains a long-term aspiration contingent on 'strategic stability' that cannot be reduced to a fixed date without risking security relationships built around extended deterrence. From the NNWS seat, the same structure is enforced extraction: a bargain accepted in 1968 on the understanding that non-proliferation and disarmament were reciprocal halves of one deal, where only one half was ever made binding. The engine computing divergent per-seat classifications from these structural declarations is the intended output — this story does not resolve the divergence, it documents the reading that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS sit at the beneficiary end: they retain the security value of their arsenals, control the interpretive consensus process that adjudicates their own compliance, and bear no verification burden proportionate to what they impose on NNWS. IAEA safeguards apparatus is a secondary beneficiary — its institutional mandate and budget derive almost entirely from monitoring horizontal proliferation, giving it structural interest in the asymmetry's persistence even though it does not set the interpretive rule itself. NNWS and their civilian nuclear industries sit at the target end: real, audited restraint obligations against an unenforceable promise. Disarmament advocacy coalitions are excluded from the operative interpretive venue (the P5-veto consensus review process) even though external legal authority (ICJ 1996) supports their reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (freezing the nuclear club, preventing horizontal spread) remains substantially live — proliferation risk has not disappeared, which is why the coordination function documented in six_questions is real and not merely rhetorical. What has drifted is the enforcement asymmetry: the mandate to eventually eliminate arsenals has not been mandatrophied away by exogenous events, it has been mandatrophied by fifty-plus years of NWS-controlled interpretation refusing to let the aspirational half mature into anything actionable, while the restraint half hardened into an intrusively verified permanent status. This reading treats that non-maturation as itself the intended, stable equilibrium rather than a failure to be fixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness_ambiguity,
    'Does ''undertake to pursue negotiations in good faith... at an early date'' create a binding, judicially cognizable obligation with a discoverable breach point, or is it structurally aspirational language whose fulfillment is permanently deferrable by the obligated parties themselves?',
    'The 1996 ICJ Advisory Opinion found unanimously that a good-faith obligation to conclude negotiations exists, but the NPT''s own review conference consensus mechanism has never operationalized a breach standard or remedy. Resolution would require either an binding tribunal ruling inside the NPT framework itself (which does not exist) or state practice converging on an enforceable interpretation, neither of which has occurred in five decades.',
    'If the binding reading prevails, the nws_reading''s classification would shift toward snare (extraction with no coordination cover remaining) as continued non-fulfillment would constitute an ongoing breach rather than exercise of interpretive discretion. If the aspirational reading holds structurally, tangled_rope with rising extractiveness is the accurate classification, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_bindingness_ambiguity, conceptual, 'Whether Article VI is binding law or aspirational language — the kernel''s central interpretive fork.').

omega_variable(
    modernization_vs_reduction_evidence,
    'Do NWS arsenal modernization programs (submarine-launched systems, low-yield warheads, delivery system upgrades across all nine possessor states in the 2010s-2020s) constitute evidence against good-faith pursuit of disarmament negotiations, or are they consistent with maintaining minimum deterrence during an aspirational, non-timebound transition?',
    'Independent arms-control tracking (SIPRI Yearbook, Federation of American Scientists Nuclear Notebook) documents warhead counts, delivery system programs, and expenditure trends across the interval; comparing trajectory against any NWS-declared disarmament benchmark would test whether the aspirational claim is being pursued or merely asserted.',
    'Sustained modernization alongside stagnant or rising total capability would corroborate the T17-style extraction-accumulation hypothesis this story''s rising extractiveness series already reflects; a genuine downward trajectory in some NWS arsenals (as has occurred for US/Russia under bilateral arms control separate from the NPT) complicates a uniform NWS-as-static-extractor narrative and should be reflected in any future revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_vs_reduction_evidence, empirical, 'Whether observed NWS arsenal trends corroborate or undercut the aspirational-compliance claim.').

omega_variable(
    safeguards_budget_asymmetry_naturalness,
    'Is the IAEA''s near-exclusive verification focus on NNWS programs a natural consequence of where proliferation risk is concentrated, or a constructed allocation that itself reflects and reinforces the NWS-favorable reading of the treaty''s obligations?',
    'Comparative analysis of IAEA budget allocation, staffing, and inspection-hours directed at NNWS safeguards versus any NWS-directed verification (which is essentially voluntary and minimal under NWS-specific safeguards agreements) would establish whether the asymmetry tracks actual proliferation risk distribution or institutional path dependency from the treaty''s founding asymmetry.',
    'If the allocation is risk-proportionate, the IAEA''s beneficiary status here is incidental rather than structural. If it is path-dependent on the founding asymmetry, the IAEA becomes a more active co-beneficiary of the NWS reading''s persistence, strengthening the tangled_rope classification''s enforcement-apparatus component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safeguards_budget_asymmetry_naturalness, empirical, 'Whether verification-resource asymmetry is risk-driven or reading-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nws_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nws_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nws_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nws_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nws_reading, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nws_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of the npt_treaty_text kernel. npt_treaty_text__nnws_reading authors the inverse premise (Article VI binding, non-proliferation conditional) with a different beneficiary/victim structure and a lower ε for the non-proliferation half assessed on its own terms. npt_treaty_text__withdrawal_threshold_reading addresses the orthogonal Article X threshold dispute. All three share the treaty text as their common kernel but instantiate structurally distinct constraints per the ε-invariance principle; none averages or references the others' metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
