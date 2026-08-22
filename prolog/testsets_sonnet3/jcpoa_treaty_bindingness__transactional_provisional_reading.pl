% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story instantiates the transactional-provisional reading of the
 *   JCPOA kernel: the arrangement is treated as a non-binding political
 *   commitment, revocable at the sole discretion of any party upon a
 *   unilateral determination that the other side is acting in bad faith.
 *   Under this reading, sanctions relief was never a treaty obligation but a
 *   contingent bargain that persists only so long as domestic political
 *   calculus favors it. This is NOT the binding-multilateral reading (which
 *   would treat withdrawal as a breach requiring dispute-resolution process)
 *   nor the graduated-compliance reading (which would tie sanctions
 *   reimposition to proportional violation assessment). Those are separate
 *   constraints, linked here only structurally.
 *
 * KEY AGENTS:
 *   - withdrawing_state_executive: primary agenda-setter (institutional/arbitrage) — declares bad faith and reimposes sanctions unilaterally
 *   - domestic_deal_opponent_coalition: primary domestic beneficiary (organized/mobile) — captures political win from unilateral reversal
 *   - regional_rival_states: secondary beneficiary (powerful/mobile) — benefits from renewed pressure on adversary
 *   - iranian_civilian_population: primary target (powerless/trapped) — bears sanctions cost with no standing in the determination
 *   - remaining_jcpoa_signatories: institutional payer (institutional/constrained) — bears secondary-sanctions exposure without having consented to withdrawal
 *   - iaea_verification_regime: institutional payer (institutional/constrained) — verification function rendered moot by political determination
 *   - international_law_scholars: analytical observer — assesses whether transactional characterization is genuine or retroactively constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.62).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.4).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '99bad1a4-96d9-4e97-b5e5-187dbf750ebd').
narrative_ontology:cs_kernel_codification('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', distributed).
narrative_ontology:cs_authority_grounding('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', distributed).
narrative_ontology:cs_reading_relation('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', foundational, unilateral_sovereign_discretion_over_nonratified_commitments).
narrative_ontology:cs_axiom_status(unilateral_sovereign_discretion_over_nonratified_commitments, holdable).
narrative_ontology:cs_axiom_grounding('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', unilateral_sovereign_discretion_over_nonratified_commitments, conventional).
narrative_ontology:cs_axiom('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', secondary, bad_faith_determination_requires_no_multilateral_adjudication).
narrative_ontology:cs_axiom_status(bad_faith_determination_requires_no_multilateral_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', bad_faith_determination_requires_no_multilateral_adjudication, instrumental).
narrative_ontology:cs_created_at('99bad1a4-96d9-4e97-b5e5-187dbf750ebd', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opponent_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, remaining_jcpoa_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_regime).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, executive_sovereign_prerogative_over_nonbinding_arrangements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the unilateral power to declare the other party in bad faith and reimpose sanctions without seeking consensus from co-signatories. Frames the JCPOA as a non-binding political commitment rather than a ratified treaty, which removes any domestic legal constraint on withdrawal. Captures domestic political credit for a hardline reversal and removes an arrangement its coalition opposed from inception.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive, agenda_setter,
    institutional, biographical, arbitrage, global).

% Lobbied against the original agreement and benefits directly from its unilateral characterization as revocable at will; gets to claim vindication and shape successor policy (sanctions legislation, defense appropriations) without needing multilateral agreement or new legislative ratification.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_deal_opponent_coalition, beneficiary,
    organized, biographical, mobile, national).

% Benefit from renewed sanctions pressure on a regional adversary and from the precedent that multilateral nuclear arrangements can be unwound by a single determined party; lobby the withdrawing state's domestic coalition and supply intelligence framing bad faith.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_rival_states, beneficiary,
    powerful, generational, mobile, regional).

% Bears the economic consequences of reimposed sanctions — currency collapse, medicine and goods shortages, employment contraction — triggered by a determination made in a foreign capital without any adjudicative process it can appeal to or exit from. Has no standing in the withdrawal decision at all.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civilian_population, payer,
    powerless, biographical, trapped, national).

% Continue to treat the framework as binding and attempt to preserve it through secondary mechanisms (special payment vehicles, continued verification cooperation) but bear secondary-sanctions exposure and diplomatic cost from a withdrawal they did not consent to and had no mechanism to block. Their consent was structurally irrelevant to the unilateral exit.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, remaining_jcpoa_signatories, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, remaining_jcpoa_signatories, excluded).

% Loses access, monitoring continuity, and enforcement leverage once the framework it certified compliance against is unilaterally voided; its technical verification work is rendered moot by a political determination it had no role in making, even where its own reporting showed continued compliance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_regime, payer,
    institutional, generational, constrained, global).

% Debate whether the JCPOA's status as a political commitment (rather than a ratified treaty under domestic constitutional procedure) genuinely licenses unilateral bad-faith determination, or whether that characterization is itself a retroactive construction serving withdrawal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_executive).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading, the framework coordinates a temporary, revocable transactional exchange — sanctions relief for verified nuclear restraint — that persists only as long as each party's own national interest calculus continues to favor it; no permanent mutual obligation is coordinated, only a contingent bargain.
% TRANSFER_FUNCTION: Moves sanctions relief and economic access to Iran in exchange for verified restraint, but structures the relief as revocable at the sole discretion of the granting state's determination of bad faith — meaning the transfer can be unilaterally reversed without multilateral process, shifting reimposed costs onto Iran's population and onto co-signatories who relied on the arrangement's continuity.
% ABSENT_VOICES: The Iranian population bears the sanctions consequences of the determination but has no seat in the bad-faith adjudication. Remaining signatories who structured their own economic and diplomatic commitments around the framework's continuity are treated as bystanders to a decision made entirely within the withdrawing state's domestic process.
% DISAPPEARANCE_RATIONALE: If the unilateral-voidability premise were rejected in favor of a binding-consensus reading, sanctions reimposition would require multilateral dispute-resolution process rather than a single executive determination; the withdrawing state's domestic coalition would lose its principal lever, and Iran's population would face sanctions risk contingent on a collective finding rather than one government's political cycle.
% FOUNDING_PROBLEM: The original 2015 arrangement was built to solve a proliferation-risk problem — halting or substantially delaying a nuclear weapons capability — through verified restraint in exchange for economic relief, in a context where a ratified multilateral treaty was politically unattainable in the withdrawing state's domestic legislature.
% FOUNDING_PROBLEM_CORROBORATION: IAEA verification reports (an outside technical body, not a party to either government's political calculus) attested continued Iranian compliance at the time of the withdrawing state's bad-faith determination, directly contradicting the stated justification. Independent nonproliferation researchers and former negotiators from third countries not party to the domestic political dispute have stated the proliferation-restraint problem the framework solved was still substantially live at withdrawal, and that the transactional-voidability reading was adopted retroactively to license a withdrawal decided on other domestic political grounds.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at T=10) reflects that once the bad-faith determination is made, sanctions reimposition transfers substantial economic cost onto the Iranian population and diplomatic/secondary-sanctions cost onto remaining signatories, with no adjudicative process standing between the determination and the cost transfer. Suppression is moderate (0.4) rather than high because the reading itself is a legal characterization dispute rather than a coercively enforced arrangement — the 'suppression' here is the foreclosure of alternative characterizations (binding treaty, graduated compliance) once one state's domestic institutions settle on the transactional reading and act on it. Theater ratio (0.45) captures that a substantial share of the withdrawal's justificatory apparatus (formal 'bad faith' findings, congressional certifications) performs legal process without the underlying adjudicative substance a genuine dispute-resolution mechanism would provide — IAEA reports showing compliance were available and contradicted the announced justification. Accessibility collapse is comparatively low (0.35): the binding-multilateral and graduated-compliance readings remain live, contested alternatives right up to and after the withdrawal — this reading has not achieved the kind of settled, alternative-foreclosing status a mountain or even a stable rope would show. Resistance is high (0.7): remaining signatories, IAEA, and international law scholars actively contested the unilateral characterization at the time.
 *
 * PERSPECTIVAL GAP:
 *   From the withdrawing executive's seat, this operates as legitimate sovereign discretion over a non-binding political arrangement — a rope, even a scaffold with an implicit sunset built into 'as long as it serves interest.' From the Iranian population's seat and the remaining signatories' seat, the same structure operates as unilateral extraction dressed in legal process: sanctions reimposed on a determination made without their participation, following continued compliance reporting from the IAEA. The engine computing different seat-level types from the same structural data models exactly this divergence; it is not resolved by picking a side.
 *
 * DIRECTIONALITY LOGIC:
 *   The withdrawing executive and its domestic coalition sit near the full-beneficiary end: they hold arbitrage-grade exit (a unilateral determination itself IS the exit) and capture political and strategic benefit. Regional rival states similarly benefit without bearing exit costs. The Iranian population sits at the full-target end: trapped exit, zero voice in the determination, direct and severe cost bearing. Remaining signatories and the IAEA sit as constrained payers — institutionally powerful but unable to prevent the unilateral act, bearing reputational and functional costs from an outcome outside their control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (halting proliferation risk via verified restraint) is authored as contested rather than flatly dead: IAEA verification — a corroborating source outside the withdrawing state's domestic coalition — reported continued compliance at the time of withdrawal, suggesting the founding problem remained substantially live by the technical body's own account, while the withdrawing state's domestic narrative held it dead or irrelevant. This mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) is exactly the kind of divergence the R5 genealogy interview is designed to surface: a genealogy corroborated only by the beneficiary coalition would have produced a flattering, uncontested founding-problem narrative; corroboration from IAEA and third-country former negotiators breaks that self-report.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jcpoa_kernel_reading_indeterminacy,
    'Is the JCPOA a binding multilateral treaty (binding_multilateral_reading), a graduated reciprocal compliance regime (graduated_compliance_reading), or a provisional transactional bargain voidable on unilateral bad-faith determination (this reading)? The underlying text (a UN Security Council-endorsed political commitment, not a ratified treaty under most domestic constitutional procedures) is genuinely compatible with more than one of these framings, and different parties adopted different readings from the outset.',
    'This is not resolvable by further evidence about the JCPOA text alone — it is a framing dispute over what kind of commitment a UNSC-endorsed political agreement constitutes absent domestic treaty ratification. Comparative international-law analysis of similar non-treaty multilateral commitments and their post-hoc characterization by withdrawing parties would bear on precedent but not settle the question.',
    'Under the transactional-provisional reading (this story), unilateral withdrawal is a permitted exercise of sovereign discretion producing low suppression and a rope/tangled-rope character. Under the binding-multilateral reading, the same withdrawal act constitutes a breach requiring consensus-based remedy, producing a substantially more extractive, more suppressive snare-like classification for the withdrawing state''s seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jcpoa_kernel_reading_indeterminacy, conceptual, 'The kernel-level indeterminacy between binding, graduated, and transactional-provisional readings of JCPOA bindingness; this story instantiates only the transactional-provisional reading.').

omega_variable(
    bad_faith_determination_process_integrity,
    'Was the withdrawing state''s ''bad faith'' determination a genuine finding based on evidence contradicting IAEA verification, or a retroactive legal characterization constructed to justify a withdrawal decided on other domestic political grounds?',
    'Comparison of the timeline and evidentiary basis of the bad-faith determination against contemporaneous IAEA compliance reporting; declassified internal deliberative records, where available, on when and why the withdrawal decision was actually made relative to the compliance evidence.',
    'If the determination substantially preceded or ignored contradicting IAEA evidence, this reading''s coordination claim (''the framework only persists as long as good faith continues'') is a cover story for extraction decided on other grounds — pushing the classification toward snare. If the determination genuinely tracked new evidence of violation, the transactional-provisional reading''s coordination function is more substantively real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_process_integrity, empirical, 'Whether the bad-faith finding was evidentiary or retroactively constructed to license a predetermined withdrawal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.25).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.18).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_sanctions_reimposition_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_mandate_continuity).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the jcpoa_treaty_bindingness kernel: binding_multilateral_reading (JCPOA as consensus-bound treaty), graduated_compliance_reading (proportional graduated enforcement), and this transactional_provisional_reading (unilaterally voidable political bargain). Each reading has its own ε, beneficiary/victim structure, and type — they are not the same constraint measured differently; the label 'the JCPOA' colloquially covers all three. Link via cs_structure.reading_relations, not via shared metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
