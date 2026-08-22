% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: Article X Withdrawal Threshold — Sovereignty-Preservation Reading
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story authors the sovereignty-preservation reading of Article X's
 *   withdrawal clause within the NPT kernel: 'extraordinary events
 *   jeopardizing supreme interests' is treated as a low, largely self-judged
 *   threshold. Under this reading, the clause is a genuine coordination
 *   achievement (it made near-universal accession possible) that has become
 *   entangled with asymmetric extraction — threshold-capable and
 *   threshold-seeking states derive real hedge value from the ambiguity,
 *   while the regime architects, IAEA verification apparatus, and non-nuclear
 *   neighbors bear the diffuse cost of a weakening deterrent. The North
 *   Korean 2003 withdrawal, never referred for Security Council enforcement,
 *   is the operative precedent that concretized the low-threshold reading in
 *   state practice even though the treaty text alone is genuinely ambiguous
 *   between readings. This is a DIFFERENT constraint from the nws_reading
 *   (non-proliferation as binding on NNWS) and the nnws_reading (disarmament
 *   as binding under Article VI) — those readings concern the substantive
 *   obligations of the treaty; this one concerns the EXIT clause's threshold,
 *   a structurally distinct claim with its own ε, its own beneficiary/victim
 *   structure, and its own persistence dynamics.
 *
 * KEY AGENTS:
 *   - threshold_capable_states: Primary beneficiary (powerful/arbitrage) — extracts hedge-value from ambiguity
 *   - iran_nuclear_program: Concrete beneficiary and agenda-shaping actor (powerful/arbitrage) — invokes the low threshold in negotiation
 *   - non_proliferation_regime_architects: Primary payer (institutional/constrained) — bears erosion of deterrent value
 *   - iaea_verification_apparatus: Institutional payer (institutional/constrained) — bears verification-credibility cost
 *   - un_security_council: Excluded institutional actor with nominal but unexercised gatekeeping role
 *   - treaty_interpretation_scholars: Analytical observer — documents but cannot resolve the ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "Article X Withdrawal Threshold — Sovereignty-Preservation Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, 'bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d').
narrative_ontology:cs_kernel_codification('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', fixed_text).
narrative_ontology:cs_authority_grounding('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', distributed).
narrative_ontology:cs_reading_relation('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', foundational, withdrawal_threshold_is_self_judged).
narrative_ontology:cs_axiom_status(withdrawal_threshold_is_self_judged, holdable).
narrative_ontology:cs_axiom_grounding('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', withdrawal_threshold_is_self_judged, conventional).
narrative_ontology:cs_axiom('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', secondary, sovereign_exit_right_outranks_regime_stability).
narrative_ontology:cs_axiom_status(sovereign_exit_right_outranks_regime_stability, holdable).
narrative_ontology:cs_axiom_grounding('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', sovereign_exit_right_outranks_regime_stability, deontological).
narrative_ontology:cs_reference_frame('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', drafters_narrow_existential_safety_valve).
narrative_ontology:cs_drift_state('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', post_dprk_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bdd40cde-5fd6-4ac3-a0f1-f4b267b85a0d', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_capable_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, states_seeking_latent_hedge_capacity).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, iaea_verification_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced fuel-cycle infrastructure (enrichment or reprocessing) short of a weapon read Article X's 'extraordinary events jeopardizing supreme interests' language as a low, self-judged threshold. This preserves a credible exit option: the treaty's own text lets them frame any future move toward weaponization as a lawful withdrawal rather than a violation, and the ambiguity itself has bargaining value even if withdrawal is never exercised.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_capable_states, beneficiary,
    powerful, generational, arbitrage, national).

% Cites Article X's self-judging supreme-interests standard when negotiating enrichment limits, using the credible threat of withdrawal (and North Korea's 2003 precedent of a stated withdrawal that produced no enforcement consequence) as negotiating leverage. Benefits directly from a low-threshold reading that keeps exit cheap and enforcement uncertain.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program, agenda_setter).

% North Korea's 2003 announced withdrawal — never referred to the UN Security Council for enforcement action, followed by open weapons development — functions as the operative case law establishing that a self-judged withdrawal claim, once asserted, faces no reliable institutional check. This precedent is the mechanism, not an actor, but it structures every subsequent threshold reading.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, dprk_precedent, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__withdrawal_threshold_reading, dprk_precedent).

% The P5 and treaty depositary states drafted Article X's three-month notice and 'circumstances' clause as a safety valve for genuinely existential threats, not as a routine hedge. Each unrebutted invocation of the low-threshold reading (DPRK) erodes the treaty's deterrent value against the next threshold-capable state; they bear the diffuse cost of a weakening non-proliferation norm they built and depend on.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects, payer,
    institutional, civilizational, constrained, global).

% States in the same region as a threshold-capable state (South Korea and Japan relative to DPRK; Gulf states relative to Iran) bear the security externality of an ambiguous withdrawal pathway without controlling it. A neighbor's low-threshold exit can trigger their own reconsideration of NPT membership, but they have no vote over how Article X is read by their neighbor.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states, payer,
    moderate, generational, trapped, regional).

% The IAEA's safeguards regime is designed around continuous verification; a low withdrawal threshold that can be invoked with three months' notice and minimal institutional review undercuts the agency's ability to maintain assurance of non-diversion, since a state can exit cleanly before any violation is confirmed. Bears the institutional cost of maintaining credibility for a verification system whose object can depart on short, self-judged notice.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_verification_apparatus, payer,
    institutional, generational, constrained, global).

% Article X requires notice 'to the Security Council' but assigns it no binding gatekeeping role over whether the stated circumstances actually meet the threshold — the Council can discuss but has never blocked or overturned a withdrawal notice. Structurally positioned to arbitrate the threshold question but without the treaty language, or the political will demonstrated in 2003, to exercise that role.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_security_council, excluded,
    institutional, immediate, analytical, global).

% Legal scholars and arms-control analysts debate whether 'extraordinary events' is a self-judged standard (sovereignty reading) or subject to objective, reviewable determination (regime-stability reading), drawing on VCLT interpretive principles and the DPRK case. They document the ambiguity without having the power to resolve it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, treaty_interpretation_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X's withdrawal clause solves a genuine problem: no sovereign state will accept a treaty with no exit at all, so a notice-and-waiting-period mechanism lets states leave if supreme security interests are genuinely jeopardized, preserving voluntary membership as the basis of near-universal accession.
% TRANSFER_FUNCTION: The low-threshold, self-judging reading moves negotiating leverage and hedge-option value from the collective non-proliferation regime to individual threshold-capable states — each unchallenged low-threshold invocation transfers credibility away from the treaty's deterrent function and toward the withdrawing state's bargaining position.
% ABSENT_VOICES: The UN Security Council is nominally in the room (notice is addressed to it) but has never exercised a substantive check; regional neighbors of a withdrawing state have no formal standing in the Article X process despite bearing the security consequences most directly.
% DISAPPEARANCE_RATIONALE: If the low-threshold reading were foreclosed and a high, objectively-reviewable threshold were institutionally settled instead, several threshold-capable states would lose a credible exit option they currently hold in reserve, altering negotiating dynamics around enrichment limits and inspection access; conversely if the ambiguity were resolved toward an even lower, entirely self-judged standard, the deterrent value of NPT membership itself would further erode.
% FOUNDING_PROBLEM: Drafters needed language that would let sovereign states join a permanent-seeming non-proliferation treaty despite the possibility that circumstances might someday genuinely threaten a state's survival — a treaty with no exit clause could not have achieved near-universal accession in 1968.
% FOUNDING_PROBLEM_CORROBORATION: Treaty architects and P5 depositary-state officials attest that the clause was meant for genuinely extraordinary existential threats and that its function has drifted toward routine leverage; independent legal scholarship (e.g. Carnegie Endowment and SIPRI analyses of the DPRK withdrawal) corroborates that the threshold has never been tested by an empowered reviewing body, supporting a reading from outside the beneficiary states that the founding problem's narrow scope has been overtaken by strategic practice.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.52 across the interval, tracking the shift from a text that was genuinely ambiguous at signing (1968-1990s) to a text whose low-threshold reading became load-bearing state practice after the DPRK 2003 precedent went unenforced. Suppression sits at a moderate 0.58 — not the near-total suppression of a snare, because rhetorical resistance (Security Council statements, NPT Review Conference language) is real, but substantial because no institutional mechanism has ever actually blocked or reviewed an invoked withdrawal. Theater ratio at 0.40 reflects that a meaningful share of diplomatic activity around Article X (Review Conference final documents reaffirming 'universality' and 'commitment to nonproliferation') is now performative reaffirmation rather than functional threshold enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold-capable states and Iran are coded as beneficiaries with arbitrage-grade exit: the low-threshold reading is valuable to them whether or not they ever invoke it, because its mere availability is a bargaining chip (d near the beneficiary end). Regime architects and the IAEA are payers with constrained exit — they cannot unilaterally tighten the threshold without treaty amendment requiring consensus they cannot secure, and their institutional survival depends on the norm the ambiguity erodes (d near the target end). Neighboring non-nuclear states are trapped payers: they bear regional security externalities of a neighbor's exit option with zero formal standing in the Article X process.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification, rather than snare, matters here because Article X's withdrawal clause genuinely IS solving a coordination problem — no state joins a treaty it cannot leave, and the clause's existence (not its ambiguity) is why near-universal accession was achievable. Classifying this purely as extraction would mislabel the coordination function that made the NPT possible at all. But the SPECIFIC sovereignty-preservation reading of the threshold, as consolidated by unrebutted state practice, has become asymmetrically extractive: it transfers real security value to threshold-capable states at the expense of the regime's collective deterrent credibility, and this transfer requires active (if largely rhetorical) enforcement effort by regime architects to contain. Both the coordination function and the extraction are real and coexist in the same clause — the tangled_rope form, not snare or rope alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_judging_vs_objective_threshold,
    'Is Article X''s ''extraordinary events jeopardizing the supreme interests of the Party'' language properly read as a self-judged standard (each state alone determines whether its own interests are jeopardized) or as an objective standard subject to external review by the depositary states, the Security Council, or the treaty membership collectively?',
    'A future contested withdrawal in which the Security Council or an NPT Review Conference actually convenes a substantive threshold review (rather than merely receiving notice) would establish institutional practice resolving this; absent that, VCLT Article 31-32 interpretive analysis of drafting history and subsequent practice remains the only resolution path, and it currently supports both readings depending on which practice (drafting intent vs. DPRK precedent) is weighted.',
    'If objective review is vindicated, the low-threshold reading this story authors becomes structurally foreclosed and the constraint would reclassify toward a tighter rope with less extraction available to threshold states; if the self-judging reading is vindicated by further unrebutted practice, extractiveness rises further and the classification drifts toward snare as the coordination function attenuates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_judging_vs_objective_threshold, conceptual, 'Whether Article X establishes a self-judged or objectively reviewable withdrawal standard.').

omega_variable(
    dprk_precedent_generalizability,
    'Does the DPRK 2003 withdrawal''s lack of enforcement consequence establish a generalizable precedent for other threshold-capable states, or was it a case-specific failure of Security Council politics (China/Russia veto dynamics) that would not necessarily recur for a different withdrawing state?',
    'Observing whether a future withdrawal notice by a different state (with different Security Council permanent-member alignments) receives different treatment would test generalizability; absent a new case, this remains a matter of comparative case analysis among international law scholars.',
    'If DPRK''s outcome is idiosyncratic rather than generalizable, the beneficiary value threshold-capable states derive from the ''precedent'' is overstated and the effective extraction is lower than the low-threshold reading assumes; if generalizable, the extraction this story measures likely understates the clause''s erosion trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dprk_precedent_generalizability, empirical, 'Whether the DPRK non-enforcement precedent generalizes to other potential withdrawing states.').

omega_variable(
    committer_framing_alternative,
    'Is the coordination-vs-extraction framing of this reading itself under-determined by an alternative framing where the true kernel element is not the withdrawal threshold''s textual ambiguity but the Security Council''s unexercised gatekeeping authority — i.e., the constraint could instead be authored as centered on Security Council institutional passivity rather than treaty-text ambiguity?',
    'Compare classification outcomes under a hypothetical alternative story authored with the Security Council''s Article X notice-receipt role as the primary kernel element rather than the treaty text''s threshold language; if the classification diverges substantially, the two framings are tracking different structural facts and should be decomposed into separate stories.',
    'If the Security Council framing were adopted instead, the beneficiary/victim structure would likely shift toward P5-veto-power dynamics as the primary extraction mechanism rather than treaty-text ambiguity per se, potentially reclassifying this constraint or splitting it into a further sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether treaty-text ambiguity or Security Council institutional passivity is the more fundamental kernel element for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(npt__be_t30, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(npt__be_t50, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(npt__su_t30, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(npt__su_t50, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the npt_treaty_text kernel. nws_reading and nnws_reading concern the treaty's substantive obligations (non-proliferation vs. disarmament as the binding term); this story concerns the exit mechanism's threshold. All three share the same underlying kernel text but instantiate structurally distinct claims with different ε, different beneficiary/victim sets, and different persistence dynamics — per the ε-invariance principle they are authored as separate stories rather than one story with an observable-selection parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
