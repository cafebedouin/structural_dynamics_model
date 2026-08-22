% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereign Prerogative
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the withdrawal-sovereignty reading of the NPT
 *   kernel: Article X's three-month-notice exit clause is read as a
 *   legitimate, unconditional exercise of state sovereignty, and treaty
 *   compliance generally is read as contingent on the signatory's assessment
 *   of its security environment rather than as an unconditional legal bond.
 *   This is structurally distinct from the oligopoly-enforcement reading
 *   (which treats Articles I-II as the binding core and Article VI as
 *   aspirational) and the reciprocal-disarmament reading (which treats
 *   Article VI as binding and horizontal/vertical nonproliferation as a
 *   reciprocal bargain) — those are separate constraints with separate ε
 *   values, linked here only by network reference. Under this reading, the
 *   coordination function (near-universal accession secured by an escape
 *   valve) is real, but the same clause that made ratification possible now
 *   allows threshold states to extract leverage from the credible threat of
 *   exit, at the cost of the regime's compliance-inducing credibility and the
 *   assurance non-nuclear states relied on.
 *
 * KEY AGENTS:
 *   - threshold_states: primary beneficiary (powerful/arbitrage) — extracts leverage from latent withdrawal option without exercising it
 *   - national_security_establishments: agenda_setter (institutional/mobile) — administers the sovereignty framing domestically and internationally
 *   - nonproliferation_regime_stability: primary payer, non-agent (institutional/trapped) — the compliance norm degraded by normalized withdrawal threats
 *   - non_nuclear_weapon_states_relying_on_regime: secondary payer (moderate/constrained) — bore the cost of forgoing weapons on a promise now revealed as revocable
 *   - iaea_and_treaty_depositaries: analytical observer (institutional/analytical) — administers notice but cannot adjudicate legitimacy
 *   - dprk_style_withdrawing_states: excluded (moderate/mobile) — cited as precedent but their substantive grievance is not adjudicated by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.31).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereign Prerogative").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '0d84a856-d426-46a5-a596-035ef8939777').
narrative_ontology:cs_kernel_codification('0d84a856-d426-46a5-a596-035ef8939777', fixed_text).
narrative_ontology:cs_authority_grounding('0d84a856-d426-46a5-a596-035ef8939777', distributed).
narrative_ontology:cs_reading_relation('0d84a856-d426-46a5-a596-035ef8939777', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('0d84a856-d426-46a5-a596-035ef8939777', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('0d84a856-d426-46a5-a596-035ef8939777', foundational, unconditional_exit_right_preserves_sovereignty).
narrative_ontology:cs_axiom_status(unconditional_exit_right_preserves_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0d84a856-d426-46a5-a596-035ef8939777', unconditional_exit_right_preserves_sovereignty, conventional).
narrative_ontology:cs_axiom('0d84a856-d426-46a5-a596-035ef8939777', secondary, treaty_compliance_contingent_on_security_environment).
narrative_ontology:cs_axiom_status(treaty_compliance_contingent_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('0d84a856-d426-46a5-a596-035ef8939777', treaty_compliance_contingent_on_security_environment, instrumental).
narrative_ontology:cs_reference_frame('0d84a856-d426-46a5-a596-035ef8939777', id_1968_ratification_bargain_escape_valve).
narrative_ontology:cs_drift_state('0d84a856-d426-46a5-a596-035ef8939777', post_1993_withdrawal_threat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d84a856-d426-46a5-a596-035ef8939777', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, national_security_establishments).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced nuclear fuel-cycle capability that remain formally within the NPT while holding the Article X withdrawal clause as a live option. The clause's existence lets them extract technology-sharing, security guarantees, and diplomatic deference from other parties who must price in the possibility of withdrawal-triggered breakout. They benefit from the threat without exercising it.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    powerful, generational, arbitrage, national).

% Defense and foreign ministries in signatory states that invoke 'supreme national interest' language to preserve maximum future flexibility. They administer the domestic legal and rhetorical apparatus that keeps withdrawal live as a bargaining chip, and they collect the diplomatic leverage that flexibility produces even when withdrawal is never exercised.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, national_security_establishments, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, national_security_establishments, beneficiary).

% The regime's compliance-inducing function depends on withdrawal being costly and rare. Every credible invocation of the sovereignty reading erodes the norm that treaty exit carries reputational and material cost, degrading the deterrent value the regime provides to all parties, including those who never intend to withdraw.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).

% States that forwent nuclear weapons programs on the strength of the NPT's collective-security promise now bear the cost when withdrawal is normalized as sovereign prerogative: their security calculus was premised on the treaty binding others as firmly as it binds them, and a credible withdrawal norm reopens the very proliferation risk their compliance was meant to close off.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_regime, payer,
    moderate, generational, constrained, global).

% Administer safeguards and receive withdrawal notifications under Article X's three-month notice requirement. They can document and publicize a withdrawal's stated reasons but have no power to block it or to adjudicate whether the 'extraordinary events' threshold has genuinely been met.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_and_treaty_depositaries, observer,
    institutional, generational, analytical, global).

% States that have actually exercised or threatened to exercise withdrawal are treated by the sovereignty reading as validating the clause's legitimacy, but their perspective on WHY they withdrew (security threat perception, alliance abandonment fears) is typically absent from the reading's own account, which treats withdrawal as a formal-legal fact rather than a substantive grievance requiring adjudication.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, dprk_style_withdrawing_states, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X preserves a formally negotiated escape valve so that no state is permanently bound to a multilateral arrangement regardless of how the security environment changes — without it, the treaty would have faced greater resistance to ratification from states unwilling to surrender sovereignty unconditionally.
% TRANSFER_FUNCTION: Moves bargaining leverage and strategic flexibility to states capable of credibly threatening withdrawal, at the expense of the collective assurance that non-nuclear states relied on when they foreclosed their own weapons programs.
% ABSENT_VOICES: States that actually withdrew or threatened withdrawal are cited as precedent by this reading but their substantive security grievances are not treated as claims requiring collective adjudication — the reading absorbs the formal act (notice given, three months elapsed) while setting aside the question of whether the underlying claim was legitimate.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading of Article X disappeared — if withdrawal were treated as breach rather than right — threshold states would lose a significant source of latent leverage and the regime's compliance-inducing credibility would strengthen; but security-establishment actors dispute whether this would improve or destabilize deterrence, since removing the exit option could also remove the pressure valve that keeps some states inside the treaty at all.
% FOUNDING_PROBLEM: Sovereign states in 1968 would not ratify a treaty that bound them unconditionally against future existential threats; Article X's withdrawal clause was the price of near-universal accession, particularly for states with latent weapons potential.
% FOUNDING_PROBLEM_CORROBORATION: Treaty negotiators' own drafting history (US and Soviet delegations' 1968 statements) attest the clause was a ratification necessity, corroborating the reading from outside any single threshold state's later invocation. Nonproliferation scholars and IAEA-adjacent analysts, however, attest that the clause has since 1993 (North Korea's first threatened withdrawal) functioned less as an unused safety valve and more as an active strategic instrument — a status shift the sovereignty reading's own beneficiaries have not conceded.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) reflects moderate-to-substantial value transfer: threshold states gain real option value and negotiating leverage from the credible threat of exit, but this reading does not claim the treaty is a pure extraction vehicle — the ratification-enabling coordination function documented in founding_problem is genuine. Suppression (0.31) is comparatively low because the clause is procedurally open (three months' notice, no veto) rather than coercive; what suppression exists is soft — diplomatic pressure applied against states that invoke the clause, not a structural bar to exit. Theater ratio (0.28) is moderate: substantial genuine deliberation occurs around invocation, but a meaningful fraction of 'sovereignty' rhetoric functions performatively to shore up domestic legitimacy for decisions made on other grounds. The 1993 inflection point (North Korea's first withdrawal threat) marks the shift from an unused safety valve to an active strategic instrument; measurements are anchored on that shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the threshold-state seat, Article X sovereignty is simply what treaty law says and always meant: unconditional exit right, no different from any treaty's default. From the non-nuclear-weapon-state seat and the regime-stability seat, the same clause reads as a structural vulnerability that was tolerable only as long as invocation stayed rare and costly — its normalization as a bargaining chip changes what the other parties actually bought when they signed. The engine's per-seat computation should reflect this asymmetry: the agenda_setter/beneficiary seats compute closer to rope/coordination, the payer seats closer to tangled_rope/extraction, from the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and national-security establishments sit near the beneficiary end: they hold the option and collect leverage from its mere existence, with mobile-to-arbitrage exit. The regime-stability norm (non-agent, institutional, trapped) sits at the target end — it cannot exit its own degradation. Non-nuclear states relying on the regime sit closer to target than beneficiary: their constrained exit (having already forgone weapons programs) means the erosion of compliance credibility falls on them disproportionately even though they are not the direct addressee of any single withdrawal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ratification required an escape valve) was real and time-bound to 1968 negotiating conditions; the founding_problem_status is authored as contested because negotiators' own history corroborates the clause's original necessity while contemporary nonproliferation scholarship corroborates a shift in function toward active strategic instrument. This is not evidence the constraint should collapse to snare — the coordination function of enabling near-universal accession persisted and still operates as a real ratification-enabling mechanism for treaty renewal contexts — but it is evidence the mandate has partially outlived the specific 1968 problem and now serves an adjacent, contested function (option-value banking) that was not the clause's original design intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_vs_binding_readings,
    'Is the NPT''s core legal character best read as a bundle of unconditional binding obligations (the oligopoly-enforcement and reciprocal-disarmament readings) with Article X as a narrow emergency exception, or as a fundamentally contingent, sovereignty-preserving arrangement in which Article X reveals the treaty''s true revocable character?',
    'State practice analysis: track whether invoking states face material and reputational costs proportionate to a ''breach of binding obligation'' framing, or costs consistent with ''exercise of a normal legal right'' framing. The North Korea 2003 case and any future invocations provide comparative data.',
    'If state practice consistently imposes breach-level costs, the sovereignty reading is descriptively weaker than this story assumes and the constraint may be better read as scaffold-with-failed-sunset or as feeding directly into the oligopoly-enforcement reading''s victim set. If costs remain low, this reading''s beneficiary structure is validated and likely to strengthen over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_reading_vs_binding_readings, conceptual, 'Whether Article X is best read as exception-to-binding-rule or as revealing the treaty''s fundamentally contingent character.').

omega_variable(
    withdrawal_threat_credibility_measurement,
    'How much of threshold states'' diplomatic leverage is actually attributable to the credible threat of Article X withdrawal versus other sources of bargaining power (existing enrichment capability, alliance structures, economic weight)?',
    'Comparative case study isolating withdrawal-threat leverage from other leverage sources — e.g., comparing negotiating outcomes for threshold states with and without recent withdrawal rhetoric, controlling for underlying capability.',
    'If withdrawal-threat leverage is a small fraction of total bargaining power, this reading''s extraction figure (0.42) may be overstated relative to the sovereignty clause''s actual causal contribution; if large, the extraction figure understates the mechanism''s importance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(withdrawal_threat_credibility_measurement, empirical, 'Isolating Article X''s specific contribution to threshold-state bargaining leverage.').

omega_variable(
    regime_stability_as_non_agent_payer,
    'Is ''nonproliferation regime stability'' a coherent non-agent entity that can meaningfully bear costs, or is this a reification that actually distributes onto specific non-nuclear-weapon states and future proliferation-affected populations?',
    'Trace the causal chain from ''regime credibility erosion'' to concrete harms (specific states reconsidering nonproliferation commitments, specific populations facing elevated proliferation risk) and determine whether the non-agent framing obscures identifiable victims who should instead be the primary payer entries.',
    'If the harm is fully traceable to identifiable state/population victims, the non_agent stakeholder should be removed or demoted and the victim structure re-authored around those concrete parties, likely increasing the story''s suppression and resistance metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_as_non_agent_payer, conceptual, 'Whether ''regime stability'' should be authored as a non-agent payer or dissolved into concrete state/population victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(npt__tr_t1993, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(npt__be_t1993, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.18).
narrative_ontology:measurement(npt__su_t1993, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1993, 0.3).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2018, 0.34).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2024, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'the NPT' per the ε-invariance principle. oligopoly_enforcement_reading treats Articles I-II as the binding kernel with Article VI as aspirational (ε likely lower, victim set centered on aspiring proliferators). reciprocal_disarmament_reading treats Article VI as binding with temporal urgency (ε likely higher on nuclear-weapon states as targets, victim set centered on non-nuclear states awaiting disarmament that never arrives). withdrawal_sovereignty_reading (this story) treats Article X as the operative kernel element and authors treaty obligations as contingent rather than binding — its distinct victim (regime stability norm) and distinct beneficiary (threshold states banking option value) do not appear in either sibling's structure. The three readings share the same treaty text but instantiate structurally different constraints with different ε values, different beneficiary/victim sets, and different classifications; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
