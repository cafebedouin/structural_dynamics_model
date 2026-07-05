% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal as Sovereign Prerogative (Security-Contingency Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the withdrawal-sovereignty reading of the NPT
 *   kernel: Article X's exit clause is treated as a legitimate, live
 *   sovereign prerogative rather than an emergency-only dead letter, and
 *   treaty obligations generally are read as contingent on the security
 *   environment rather than as unconditional. This is a distinct constraint
 *   from the oligopoly-enforcement reading (which treats Articles I-II as the
 *   binding core and Article VI as aspirational) and from the
 *   reciprocal-disarmament reading (which treats Article VI as binding with
 *   temporal urgency). Those are separate stories with separate epsilon
 *   values and separate stakeholder sets, linked here by
 *   network.affects_constraints. Under this reading specifically, extraction
 *   shifts to a genuinely different beneficiary set (threshold states holding
 *   withdrawal as option value) and a genuinely different victim (the regime
 *   stability norm itself, plus non-nuclear-weapon states who relied on
 *   near-universal, durable commitment).
 *
 * KEY AGENTS:
 *   - threshold_states: beneficiary/agenda_setter (powerful/arbitrage) — hold withdrawal as live option value
 *   - dprk: beneficiary (powerful/arbitrage) — the only state to have exercised Article X, establishing operative precedent
 *   - regime_stability_norm: payer/non-agent (institutional/trapped) — absorbs credibility erosion with no seat of its own
 *   - non_nuclear_weapon_states_relying_on_universality: payer (moderate/trapped) — bear the externality of others' retained exit option
 *   - iaea_and_safeguards_apparatus: payer/observer (institutional/constrained) — verification investments premised on durability now provisional
 *   - depositary_governments: agenda_setter (institutional/constrained) — adjudicate what counts as valid withdrawal in practice
 *   - international_law_scholars: observer (analytical) — assess rebus sic stantibus legitimacy vs. instrumental laundering
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.38).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal as Sovereign Prerogative (Security-Contingency Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'c29744ec-18cc-4689-b168-66e7c32fff27').
narrative_ontology:cs_kernel_codification('c29744ec-18cc-4689-b168-66e7c32fff27', fixed_text).
narrative_ontology:cs_authority_grounding('c29744ec-18cc-4689-b168-66e7c32fff27', distributed).
narrative_ontology:cs_reading_relation('c29744ec-18cc-4689-b168-66e7c32fff27', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('c29744ec-18cc-4689-b168-66e7c32fff27', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('c29744ec-18cc-4689-b168-66e7c32fff27', foundational, sovereign_exit_right_survives_multilateral_commitment).
narrative_ontology:cs_axiom_status(sovereign_exit_right_survives_multilateral_commitment, holdable).
narrative_ontology:cs_axiom_grounding('c29744ec-18cc-4689-b168-66e7c32fff27', sovereign_exit_right_survives_multilateral_commitment, conventional).
narrative_ontology:cs_axiom('c29744ec-18cc-4689-b168-66e7c32fff27', foundational, treaty_obligation_contingent_on_stated_security_environment).
narrative_ontology:cs_axiom_status(treaty_obligation_contingent_on_stated_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('c29744ec-18cc-4689-b168-66e7c32fff27', treaty_obligation_contingent_on_stated_security_environment, empirically_contingent).
narrative_ontology:cs_reference_frame('c29744ec-18cc-4689-b168-66e7c32fff27', conditional_sovereign_commitment_framework).
narrative_ontology:cs_drift_state('c29744ec-18cc-4689-b168-66e7c32fff27', post_dprk_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c29744ec-18cc-4689-b168-66e7c32fff27', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, states_with_latent_breakout_capacity).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_universality).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, verification_and_safeguards_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, dprk).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_and_safeguards_apparatus).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, rebus_sic_stantibus_treaty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with substantial enrichment, reprocessing, or weapons-adjacent infrastructure built up under Article IV's peaceful-use guarantee. They hold Article X withdrawal as a credible option: three months' notice plus a supreme-interests declaration is sufficient to exit with no binding penalty. They invoke deteriorating security environment language to keep the option live without exercising it, extracting deterrence value and negotiating leverage from the mere availability of exit.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, agenda_setter).

% The only state to have actually invoked Article X and left the treaty (2003), citing extraordinary events (IAEA referral, perceived hostile security posture) jeopardizing supreme national interests. Its withdrawal was contested procedurally (whether the required advance notice and Security Council notification were validly completed) but was never reversed or punished in a way that closed the precedent. It demonstrates, by example, that the exit clause is operative rather than merely textual.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, dprk, beneficiary,
    powerful, generational, arbitrage, national).

% The expectation that near-universal, durable adherence deters proliferation by making the nonproliferation baseline appear irreversible. Each time withdrawal is treated as a live sovereign option rather than a hypothetical, the credibility of universal, permanent commitment erodes; the norm has no seat of its own to defend itself and absorbs the cost silently through weakened deterrence signaling to the next potential withdrawer.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% States that renounced weapons programs on the understanding that near-universal adherence would hold, forgoing the option value threshold states retain. They cannot credibly threaten withdrawal themselves (no infrastructure, no security calculus that would be believed) and so bear the security externality when others' withdrawal threats are treated as legitimate: the value of their own restraint declines as the regime's binding character is revealed to be conditional rather than absolute.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_universality, payer,
    moderate, generational, trapped, global).

% The verification bureaucracy built to monitor compliance under an assumption of durable obligation. When withdrawal is framed as an available sovereign remedy rather than an emergency-only escape hatch, the apparatus's monitoring investments and safeguards agreements become provisional rather than load-bearing, and it must continually re-justify inspection access as if the underlying commitment might lapse at any state's discretion.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_and_safeguards_apparatus, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_and_safeguards_apparatus, observer).

% The treaty's depositary states (US, UK, Russia) administer the withdrawal notification mechanism and adjudicate, in practice, whether a state's supreme-interests declaration and procedural notice satisfy Article X. Their interpretive latitude in accepting or contesting a withdrawal (as with the DPRK case) determines whether the sovereignty reading hardens into settled practice or remains contested.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, depositary_governments, agenda_setter,
    institutional, generational, constrained, global).

% Assess whether Article X operates as a genuine safety valve consistent with customary international law on changed circumstances (rebus sic stantibus), or whether its sovereignty framing is being used instrumentally to launder proliferation decisions as lawful exits rather than treaty violations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X provides a lawful, orderly exit mechanism so that a state facing a genuine existential security threat can leave the regime through declared process rather than through clandestine breakout or forced non-compliance, preserving the treaty's legal coherence and avoiding a cliff-edge crisis.
% TRANSFER_FUNCTION: Moves option value and negotiating leverage to states capable of credibly invoking withdrawal (chiefly technologically advanced or already-suspect states), while the cost — erosion of the universality assumption that other states relied on when foregoing weapons programs — is borne diffusely by the non-nuclear-weapon states and by the verification apparatus built on an assumption of durability.
% ABSENT_VOICES: Non-nuclear-weapon states that renounced weapons programs on the premise of near-universal, effectively irreversible commitment have no seat in adjudicating what counts as a valid 'supreme interests' invocation; that adjudication runs through depositary governments and the withdrawing state itself. Future generations relying on the deterrent value of a stable regime are not present at all.
% DISAPPEARANCE_RATIONALE: If Article X were read as narrowly as possible (withdrawal essentially unavailable in practice), threshold states would lose a genuine hedge against changed security circumstances and might instead pursue clandestine paths or refuse to join future instruments — the sovereignty reading's defenders argue this. If the sovereignty reading were abolished and Article X treated as functionally dead letter, the regime's proponents argue the world would barely change in the security-environment sense but the credibility of universal commitment would strengthen. The dispute over which world is closer to actual is the kernel contest itself.
% FOUNDING_PROBLEM: States negotiating the NPT in the late 1960s would not accept an obligation with literally no exit under any circumstance — sovereign states historically retain a right to abrogate treaties when extraordinary events threaten supreme interests (reflecting customary rebus sic stantibus doctrine), and without some such clause, several prospective parties would not have signed.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and treaty-negotiation records (U.S. and Soviet negotiating archives) corroborate that Article X was a hard-won concession necessary to secure signatures from states unwilling to accept an unconditional obligation — this attestation comes from outside the states that later benefited from invoking it. Nonproliferation scholars and NPT Review Conference documents, however, attest that the article's contemporary invocation (principally by the DPRK) reflects a security posture the founding negotiators did not anticipate and that its founding rationale (avoiding total inflexibility) has been repurposed to cover a materially different situation (a state building toward weapons capacity under treaty cover, then exiting once suspected).
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).
:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at 2025) rather than severe because the withdrawal right, on its face, is a narrow and rarely-exercised clause — the coordination story (a lawful safety valve preventing worse outcomes like clandestine breakout) is genuine and not merely cover. But extraction is non-trivial and has risen since 2003 because the DPRK precedent converted a theoretical clause into a demonstrated, low-cost exit path, which measurably increases the option value threshold states hold and measurably degrades the credibility non-nuclear-weapon states rely on. Suppression is moderate (0.38): the mechanism does not coerce anyone to stay, but active diplomatic and institutional effort goes into contesting or narrowing withdrawal claims (procedural objections to DPRK's exit, calls to tighten Article X interpretation) — this is enforcement aimed at the interpretation, not classic coercive suppression of exit, and it has intensified over the interval as the precedent value of the DPRK case became clear. Theater ratio is moderate-low (0.30): review conferences produce substantial genuine diplomatic activity but an increasing share addresses the withdrawal question performatively (repeated calls to 'strengthen Article X language' without amendment, since amendment requires near-unanimous ratification that is not forthcoming).
 *
 * PERSPECTIVAL GAP:
 *   From the threshold-state seat, Article X operates as rope: a genuine, bounded coordination mechanism that lets a state exit lawfully rather than break the treaty covertly, preserving the rule-of-law character of the whole regime. From the non-nuclear-weapon-state seat and from the regime-stability-norm's structural position, the same clause operates as an asymmetric extraction: it grants option value to states with breakout-relevant infrastructure while offering nothing comparable to states that already forewent that infrastructure, and it does so through the same textual structure that supposedly forecloses proliferation. This divergence is exactly the kind of seat-relative computation the engine is built to surface rather than resolve by author fiat.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and the DPRK are declared beneficiaries because Article X's low procedural bar (three months' notice, a supreme-interests declaration accepted largely at face value) converts into real negotiating leverage and deterrent hedging value even absent exercise — this is a low-d, near-beneficiary relationship. The regime stability norm and reliant non-nuclear-weapon states are declared victims: they hold trapped exit options (a state that already renounced weapons cannot credibly threaten withdrawal to extract equivalent leverage) and bear the externality of others' credible exit threat, pushing their derived directionality toward the target end. The IAEA apparatus sits closer to symmetric-but-burdened: it is not directly extracted from in a transfer sense, but its institutional investment is devalued by the contingent reading, which the constrained exit_options and institutional power atom captures without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview captures the mandatrophy risk directly: Article X was negotiated to solve a narrow problem (no sovereign state would sign an absolutely unconditional, no-exit treaty) but its contemporary invocation addresses a materially different situation (a state that used treaty membership to build capacity, came under suspicion, and then exited using the clause as legal cover). Classifying this as tangled_rope rather than snare or mountain avoids two errors: treating the clause as pure natural-law sovereignty (which would erase the real cost to the non-nuclear-weapon states and the regime-stability norm) and treating it as pure extraction (which would erase the genuine, historically necessary coordination function of providing a lawful safety valve). The contested founding_problem_status field marks that this is precisely the unresolved question, not a settled one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_as_safety_valve_or_laundering_device,
    'Is Article X''s contemporary function best described as a genuine rebus sic stantibus safety valve consistent with customary international law, or as a mechanism that launders a proliferation decision as a lawful exit rather than a treaty violation?',
    'Comparative analysis of withdrawal invocations against the customary international law standard for changed circumstances (fundamental, unforeseen change genuinely threatening the state''s vital interests) versus the DPRK''s actual security posture and capacity trajectory at time of invocation; scholarly consensus among international law bodies outside the NPT depositary states.',
    'If the safety-valve reading holds, the coordination function is real and extraction is bounded to the option-value effect; if the laundering reading holds, the clause functions closer to pure extraction (a snare-adjacent reading) with the sovereignty framing as cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_as_safety_valve_or_laundering_device, conceptual, 'Whether Article X withdrawal functions as genuine legal safety valve or instrumental cover for proliferation.').

omega_variable(
    regime_stability_norm_victim_status,
    'Is ''regime stability'' a genuine collective good with a real victim when eroded, or is it primarily a rhetorical construct advanced by states (nuclear weapon states, depositary governments) who benefit from other states'' continued restraint?',
    'Empirical tracking of whether non-nuclear-weapon states'' proliferation calculus measurably shifted after the DPRK withdrawal precedent (e.g., changes in latent capacity investment, NPT Review Conference positions) versus remaining stable.',
    'If regime stability is a real collective good with measurable erosion effects, its designation as victim in base_properties is well-grounded; if it is primarily rhetorical, the tangled_rope classification''s asymmetric-extraction gate is weaker than authored and the constraint moves toward a contested rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_stability_norm_victim_status, empirical, 'Whether regime stability constitutes a genuine victim or a rhetorical construct.').

omega_variable(
    sovereignty_reading_framing_alternative,
    'Could this same withdrawal clause be authored instead as primarily serving depositary-government interests (a controlled, legible exit process that depositary states can monitor and contest) rather than threshold-state sovereignty — i.e., is the beneficiary better identified as the states administering the withdrawal mechanism rather than the states invoking it?',
    'Compare depositary government behavior across withdrawal attempts: if depositary states consistently use their interpretive latitude to narrow or contest withdrawal claims (as with DPRK) rather than facilitate them, the mechanism may function more as a controlled-exit gate benefiting the depositary states'' oversight interest than as unconstrained threshold-state sovereignty.',
    'Under the depositary-benefit framing, beneficiaries would shift from threshold_states toward depositary_governments, and the classification could move toward a different tangled_rope configuration with a different asymmetric-extraction pair; this is a genuine alternative framing of the same textual clause, not a different constraint, and is flagged here rather than folded into the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_reading_framing_alternative, conceptual, 'Alternative framing: depositary-government control benefit versus threshold-state sovereignty benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.22).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_treaty_1970 kernel, decomposed per the epsilon-invariance principle. oligopoly_enforcement_reading treats Articles I-II as the binding core with Article VI aspirational, producing a different beneficiary set (existing nuclear weapon states) and different epsilon. reciprocal_disarmament_reading treats Article VI as binding with temporal urgency, producing yet another beneficiary/victim structure (non-nuclear-weapon states as victims of unfulfilled disarmament promises). This story treats Article X's exit mechanism as load-bearing, producing threshold states and the regime-stability norm as the relevant beneficiary/victim pair. All three are linked here; none averages or hedges across the others' epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
