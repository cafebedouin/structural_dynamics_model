% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: NPT Article X Withdrawal Right — Sovereignty Reading (Obligations Contingent on Security Environment)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   Article X of the Non-Proliferation Treaty reserves every party a
 *   unilateral exit: three months' notice, triggered by events the party
 *   itself judges to jeopardize its supreme interests. This story
 *   instantiates the withdrawal-sovereignty reading of that structure — the
 *   reserved exit as a legitimate exercise of sovereignty, and treaty
 *   obligations as contingent on the security environment rather than
 *   absolute. The epsilon referent is the standing arrangement under contest:
 *   the NPT regime as it actually operates with the reservation in place,
 *   assessed by this reading's own lights. The reading holds the reservation
 *   legitimate and credits it with making near-universal accession possible;
 *   the structural data nonetheless show the same reservation transferring
 *   option value to exit-capable states and diluting the security value of
 *   compliance for everyone else. The degradation of the regime-stability
 *   norm named in the expected structural delta is carried here by actor
 *   seats — compliant parties and exposed neighbors — because norms are not
 *   actors and cannot occupy victim slots; the doctrines the arrangement
 *   vindicates are listed under vindicated_propositions. The claim and the
 *   metrics are independent authored facts: the reading claims a hybrid
 *   coordination-plus-transfer structure, and the metrics describe operation
 *   that has grown steadily more transfer-heavy since the first exercised
 *   exit. Sibling readings are separate stories with separate epsilon values,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - threshold_hedging_states: primary beneficiary and agenda defender (organized/arbitrage) — collects option value and polices the option's continued availability
 *   - nuclear_weapon_states: doctrinal beneficiary (institutional/arbitrage) — gains symmetry cover for contingent performance of their own undertakings
 *   - compliant_nonnuclear_parties: primary target (moderate/constrained) — absorbs the dilution of the bargain they honored
 *   - regional_neighbors_of_threshold_states: concentrated target (moderate/trapped) — bears the security externality directly
 *   - iaea_verification_system: institutional payer with secondary benefit (institutional/trapped) — loses sunk coverage on exit, gains mandate scale from universality
 *   - un_security_council: formally notified observer (institutional/analytical) — records the dispute without levers on it
 *   - regional_civilian_populations: excluded voice (powerless/trapped) — the stakes of the argument, absent from it
 *   - regime_theory_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.6).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.55).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right — Sovereignty Reading (Obligations Contingent on Security Environment)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '4ef4685e-6e22-43d2-8a70-9f96ed720d96').
narrative_ontology:cs_kernel_codification('4ef4685e-6e22-43d2-8a70-9f96ed720d96', fixed_text).
narrative_ontology:cs_authority_grounding('4ef4685e-6e22-43d2-8a70-9f96ed720d96', self_enforcing).
narrative_ontology:cs_reading_relation('4ef4685e-6e22-43d2-8a70-9f96ed720d96', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('4ef4685e-6e22-43d2-8a70-9f96ed720d96', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_axiom('4ef4685e-6e22-43d2-8a70-9f96ed720d96', foundational, supreme_interest_self_judgment_is_final).
narrative_ontology:cs_axiom_status(supreme_interest_self_judgment_is_final, holdable).
narrative_ontology:cs_axiom_grounding('4ef4685e-6e22-43d2-8a70-9f96ed720d96', supreme_interest_self_judgment_is_final, deontological).
narrative_ontology:cs_axiom('4ef4685e-6e22-43d2-8a70-9f96ed720d96', foundational, obligations_extend_only_while_security_environment_permits).
narrative_ontology:cs_axiom_status(obligations_extend_only_while_security_environment_permits, holdable).
narrative_ontology:cs_axiom_grounding('4ef4685e-6e22-43d2-8a70-9f96ed720d96', obligations_extend_only_while_security_environment_permits, instrumental).
narrative_ontology:cs_reference_frame('4ef4685e-6e22-43d2-8a70-9f96ed720d96', revocable_sovereign_bargain).
narrative_ontology:cs_drift_state('4ef4685e-6e22-43d2-8a70-9f96ed720d96', contemporary_arms_control_breakdown, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ef4685e-6e22-43d2-8a70-9f96ed720d96', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_hedging_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, compliant_nonnuclear_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regional_neighbors_of_threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_verification_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_verification_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain complete or near-complete fuel cycles and weaponization latency short of assembly. Invoke the reserved exit in Review Conference debates, lead resistance to proposals that would attach penalties to withdrawal notification, and convert the credibility of a possible exit into diplomatic leverage — concessions, verification forbearance, and security accommodations from compliance-minded counterparts. Their exit path is the option itself: they are positioned to exercise it, and exercising it is the source of their bargaining position.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_hedging_states, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_hedging_states, agenda_setter).

% Hold arsenals outside the treaty's principal renunciation obligations and serve as depositaries receiving withdrawal notifications. They invoke the same conditionality logic the reserved exit embodies to keep their own undertakings (timetabled disarmament commitments) contingent on the security environment, and they sustain the doctrine's legitimacy in depository practice. The reserved exit costs them nothing and supplies doctrinal symmetry: no obligation in the regime binds against a self-judged supreme interest.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Accepted full-scope safeguards, foregone enrichment and reprocessing or submitted to intrusive verification, and built alliances and diplomatic standing on demonstrated good faith. Each credible exit elsewhere devalues what their compliance purchases: the assurance that restraint is general. They could notify withdrawal themselves, but doing so would forfeit the standing, market access, and security relationships their compliance accumulated, so they stay and absorb the dilution of the bargain they honored.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, compliant_nonnuclear_parties, payer,
    moderate, generational, constrained, global).

% Live within missile range of states holding credible withdrawal threats. They bear the security externality of a neighbor's optionality directly and immediately: hedging next door, crisis instability, and the possibility that a notified exit converts overnight into a latent arsenal. They cannot relocate, and their protection depends on regime stability they do not control.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regional_neighbors_of_threshold_states, payer,
    moderate, immediate, trapped, regional).

% Operates the safeguards whose acceptance the reserved exit made politically possible — states joined partly because verification was not forever. It collects mandate scale and budget from near-universal coverage, but loses sunk verification investment whenever a party exits and inspectors are expelled, and it must administer whatever arrangements the parties leave behind. It cannot refuse the system it is given.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_verification_system, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_verification_system, beneficiary).

% Receives withdrawal notifications as the treaty directs, debates them, and adopts statements urging reconsideration. It possesses no mechanism to condition, delay, or reverse a notified exit; its involvement in the arrangement is formal rather than operative, and its postures record the dispute without resolving it.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% Live under the shadow of arsenals a neighbor's reserved exit could produce. They are represented only indirectly, by governments speaking in Review Conference and Council chambers; they hold no independent standing in any forum where the option's fate is argued, and their exposure is the stakes over which the argument proceeds.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regional_civilian_populations, excluded,
    powerless, biographical, trapped, regional).

% Document withdrawal precedents, model the option value of reserved exits, and trace compliance-incentive erosion across the regime's history. They publish assessments that shape elite discourse but hold no vote in the arrangement and no lever on its operation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_theory_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_hedging_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reserved exit solved the accession problem: sovereign states will not accept perpetual, irrevocable renunciation of a survival-relevant option, and reserving a unilateral departure on self-judged supreme interests made near-universal signature possible in 1968 and keeps membership open to states whose security environments shift. Every party's obligations stand, but each party retains the door.
% TRANSFER_FUNCTION: Moves bargaining leverage and risk. Exit-capable states convert latent capability into concessions and forbearance extracted from compliance-dependent counterparts; compliant parties transfer assurance-reliance onto a bargain whose value falls with every credible exit threat; a state that exercises the exit transfers its verification burden outward — onto neighbors who inherit the exposure and onto the inspectorate that loses its sunk coverage.
% ABSENT_VOICES: Civilian populations within range of a potential withdrawn-state arsenal have no seat; small compliant non-nuclear parties without Review Conference leverage object to optionality devaluing their compliance but are outmaneuvered under consensus rules; verification specialists proposing costly-exit reforms are heard and set aside. The seats that profit from the reserved exit set the agenda under which objections are entertained.
% DISAPPEARANCE_RATIONALE: If the reserved exit vanished overnight, threshold states' bargaining leverage would evaporate, compliance commitments would become credible in a way they currently are not, states contemplating accession would demand new compensation for irrevocability, and the Review Conference agenda would reorganize around the question of what a permanently bound party is owed. The regime's entire incentive architecture depends on the door remaining open.
% FOUNDING_PROBLEM: In 1968 the drafters faced the accession problem: how to obtain near-universal renunciation of nuclear weapons from sovereign states whose future security environments were unknowable, when the bedrock norm of the system is that no state irrevocably bets its survival on other states' future conduct. The reserved exit was the price of signature — demanded by non-aligned delegations and granted so that no party would be trapped by its own ratification.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the Eighteen-Nation Disarmament Committee negotiating record shows non-nuclear delegations demanding the exit reservation as an explicit condition of signature; general treaty-law practice (the fundamental-change doctrine codified in the Vienna Convention) attests that conditioned obligation is the system-wide norm, not an NPT-specific accommodation; and nonparty states publicly attest that no sovereign state signs away strategic options without reserve. None of these sources belongs to the set that captures the option's value.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.60 at interval end) is moderate and rising: the reserved exit began as a dormant insurance clause, acquired value as hedgers accumulated latency, jumped when the 2003 exit demonstrated the playbook, and continues climbing as general arms-control breakdown validates conditionality logic across the regime. Suppression (0.55) reflects the active defense the option requires — penalty proposals must be beaten back at successive Review Conferences, and the suppression_requirement series tracks that enforcement history specifically (flat before 1995, spiking after 2003, elevated since), which is why it is authored at all here. Theater (0.50) has risen with the procedural ritual surrounding exit — notification formalities, Council statements that change nothing, quadrennial reaffirmations — crossing the Goodhart watch-line at interval end while the underlying function remains real. Accessibility collapse (0.45): alternatives (penalty regimes, costly-signaling reforms, irrevocability instruments) remain visible and periodically proposed but are structurally blocked by the seats that profit from optionality. Resistance (0.60) is real and recurring. The series run on one shared eight-point grid (interval years since 1970-entry-into-force: 0=1970, 25=1995 indefinite extension, 33=2003 first exercised exit, 55=present) so every metric is authored at every examined time point; the trajectory is a monotonic ratchet, not a cycle, and the ratchet itself — each successful exit raising the option's price — is part of the transfer mechanism, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same text. From the threshold and nuclear-weapon seats the reserved exit is the regime's load-bearing feature: the reason accession happened and the reason their positions remain tenable — a coordination structure they defend. From the compliant-party and exposed-neighbor seats the identical structure operates as a standing devaluation of everything their compliance purchases: assurance priced against a bargain others may leave. The inspectorate experiences both at once — scale from universality, stranding on exit. The Council and the analyst seats see the hybrid without living either side. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold hedging states are declared beneficiaries with arbitrage-grade exit — the option is their asset — placing them near the full-beneficiary end, amplified by their agenda-setting defense of the option's availability. Nuclear-weapon states are declared beneficiaries with arbitrage exit: the doctrine costs them nothing and insures their contingent posture. Compliant non-nuclear parties are declared targets with constrained exit — they could notify withdrawal but their accumulated compliance investment makes exit ruinous, so they sit near the full-target end. Exposed neighbors are targets with trapped exit: geography, not choice. The inspectorate is dual-positioned (payer with secondary benefit), landing mid-to-high. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat, and the override surface keys on power atoms shared across seats, which would blur distinctions the declarations already draw cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereign states will not irrevocably bet survival on others' future conduct — remains live, so no mandatrophy is declared and none is resolved. The classification work here is preventive in both directions. Without the declared coordination function (accession-enabling reservation), the arrangement reads as pure extraction: exit threats pricing concessions out of compliance-dependent states, with the 2003 precedent as the smoking gun. Without the declared targets (compliers, neighbors, the inspectorate), it reads as pure coordination: the valve that made the treaty universal. The hybrid claim holds both, and the measurement series arbitrates the balance over time — extraction and theater rising together is the signature of a coordination structure accreting transfer, which is what the data show. The theater trajectory is the early-warning line: if the reservation's function atrophied into pure declaratory maintenance while the option's value kept accruing to hedgers, the hybrid would decay toward the extractive pole with the mandate dead and the form alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading of the NPT kernel (npt_treaty_1970) — the withdrawal-sovereignty reading. How would the sibling readings (oligopoly_enforcement_reading, reciprocal_disarmament_reading) redistribute the structural positions declared here?',
    'Compile the two sibling stories and compare computed per-seat classifications; convergence and divergence localize where the readings actually disagree.',
    'Under the oligopoly reading the compliant non-nuclear seats become protected beneficiaries and the nuclear-weapon seats become violators; under the reciprocal reading the nuclear-weapon seats become debtors and the reserved exit becomes a loophole degrading the bargain. Either sibling would invert parts of this story''s victim and beneficiary sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three live readings of the NPT kernel; sibling readings would reassign who counts as victim.').

omega_variable(
    supreme_interest_objectivity,
    'Does ''extraordinary events jeopardizing supreme interests'' admit any objective criterion, or is the trigger inherently self-judged and unfalsifiable?',
    'Comparative analysis of every invocation and serious contemplation of exit (the 2003 DPRK notification; states citing supreme interests in safeguards disputes) against contemporaneous, externally observable security indicators.',
    'If the trigger is purely subjective, conditionality reduces to unconstrained revocability and effective extraction runs above the authored value; if external criteria discipline invocations, extraction is bounded and the coordination side of the arrangement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supreme_interest_objectivity, empirical, 'Whether the withdrawal trigger is objectively disciplinable or purely self-judged.').

omega_variable(
    option_value_capture_distribution,
    'Who actually captures the option value generated by the reserved exit — threshold states alone, or also the nuclear-weapon states (doctrinal cover for contingent performance of their own undertakings) and ordinary compliant parties (insurance against worst-case security shifts)?',
    'Bargaining-trace analysis of Review Conference outcomes: which concessions and forbearances track credible exit threats specifically, versus generalized insurance demand common to all parties.',
    'If the nuclear-weapon states capture the larger share, the arrangement drifts toward captured extraction despite the sovereignty framing; if the value diffuses broadly across parties, the hybrid coordination-plus-transfer reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_capture_distribution, empirical, 'Distribution of the exit option''s value across the party spectrum.').

omega_variable(
    precedent_ratchet_direction,
    'Does each successful withdrawal raise the reserved exit''s value monotonically (a precedent ratchet), or does backlash eventually install penalty mechanisms that cap it?',
    'Track adoption of withdrawal-penalty provisions in successor agreements and UN Security Council practice following each withdrawal episode.',
    'A dominant ratchet validates the rising measurement series and pushes the arrangement toward pure extraction; effective backlash reverses the series and restores a coordination-dominated valuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_ratchet_direction, empirical, 'Whether withdrawal precedents ratchet the option''s value upward or provoke capping backlash.').

omega_variable(
    constraint_boundary_framing,
    'Is the standing arrangement under contest the Article X clause narrowly, or the broader doctrine that treaty obligations are contingent on the security environment wherever a state invokes it?',
    'Epsilon-invariance test: author the narrow-clause story and the broad-doctrine story as separate files and compare computed classifications; divergence indicates two constraints, not one measured two ways.',
    'The narrow framing confines scope to NPT parties and moderates scope-amplified extraction; the broad framing extends the target set to counterparties of any conditioned obligation and raises effective extraction accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_boundary_framing, conceptual, 'Framing under-determination: clause-level versus doctrine-level boundary of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(npt__tr_t25, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(npt__tr_t33, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 33, 0.38).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 55, 0.5).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(npt__be_t10, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(npt__be_t20, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(npt__be_t25, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(npt__be_t33, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 33, 0.54).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(npt__be_t55, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 55, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(npt__su_t10, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(npt__su_t20, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(npt__su_t25, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(npt__su_t33, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 33, 0.52).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(npt__su_t55, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 55, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the NPT' conflates three structurally distinct claims, each authored as its own story with its own epsilon, beneficiaries, and victims. This story instantiates the withdrawal-sovereignty reading (Article X as legitimate sovereign reservation; obligations contingent on security environment). The oligopoly-enforcement reading (horizontal proliferation prevention as the binding core) is the historically dominant institutional reading and sits upstream: this reading's successes erode its 'binding' premise without replacing it. The reciprocal-disarmament reading (Article VI as binding, urgent obligation) contests from the disarmament side; this reading's core premise contradicts its core premise such that no single framework holds both. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
