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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereignty Exercise
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   The Nuclear Nonproliferation Treaty's Article X grants all states the
 *   right to withdraw on three months' notice if 'extraordinary events'
 *   compromise their supreme interests. This constraint models one reading of
 *   that right: the reading that frames withdrawal as a legitimate
 *   sovereignty exercise, that treaty obligations are contingent on the
 *   security environment remaining stable. This reading benefits threshold
 *   states (they gain option value and strategic ambiguity) and security
 *   hedgers (whose rearmament flexibility is validated by the reading), while
 *   imposing costs on the regime's stability norm, nonnuclear states (who
 *   lose reciprocity), and disarmament advocates (whose pressure on Article
 *   VI weakens). The constraint is CLAIMED as tangled_rope because it
 *   coordinates horizontal proliferation prevention while asymmetrically
 *   extracting from those (nonnuclear states, disarmament constituencies) who
 *   bear the compliance burden without exit optionality. The measurement
 *   series tracks the reading's growing operationalization: as threshold
 *   states (Iran, North Korea, and ambiguous cases in the Middle East and
 *   Asia) have invoked withdrawal threats as negotiating leverage, the
 *   extractiveness and theater ratios have risen, modeling a constraint whose
 *   functional meaning drifts away from 'emergency exit' toward 'contingent
 *   commitment'.
 *
 * KEY AGENTS:
 *   - Threshold states (Iran, North Korea, Japan, South Korea, Egypt): possess technical capacity, face regional threats, gain option value from withdrawal credibility
 *   - Nuclear weapon states (Russia, China, USA, UK, France): maintain ambiguity about their own disarmament obligations and interpret Article X to reinforce that ambiguity
 *   - Regime stability norm: the doctrinal victim—nonproliferation depends on compliance being binding, but this reading renders it revocable
 *   - Nonnuclear states (Brazil, Indonesia, Egypt, Australia): forgo weapons; lose negotiating position when the exchange becomes contingent
 *   - Disarmament constituencies (civil-society organizations, some NNWS delegations, treaty-review bodies): advocate for Article VI as mandatory; undercut by the contingency reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereignty Exercise").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '3a93565c-9d76-4d9b-82aa-06e676988b02').
narrative_ontology:cs_kernel_codification('3a93565c-9d76-4d9b-82aa-06e676988b02', formalized).
narrative_ontology:cs_authority_grounding('3a93565c-9d76-4d9b-82aa-06e676988b02', extraction).
narrative_ontology:cs_interpretation_layer_present('3a93565c-9d76-4d9b-82aa-06e676988b02').
narrative_ontology:cs_reading_relation('3a93565c-9d76-4d9b-82aa-06e676988b02', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a93565c-9d76-4d9b-82aa-06e676988b02', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_axiom('3a93565c-9d76-4d9b-82aa-06e676988b02', foundational, state_sovereignty_exit_preeminent).
narrative_ontology:cs_axiom_status(state_sovereignty_exit_preeminent, holdable).
narrative_ontology:cs_axiom_grounding('3a93565c-9d76-4d9b-82aa-06e676988b02', state_sovereignty_exit_preeminent, deontological).
narrative_ontology:cs_axiom('3a93565c-9d76-4d9b-82aa-06e676988b02', foundational, treaty_obligations_environmentally_contingent).
narrative_ontology:cs_axiom_status(treaty_obligations_environmentally_contingent, holdable).
narrative_ontology:cs_axiom_grounding('3a93565c-9d76-4d9b-82aa-06e676988b02', treaty_obligations_environmentally_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('3a93565c-9d76-4d9b-82aa-06e676988b02', sovereign_exit_prerogative).
narrative_ontology:cs_drift_state('3a93565c-9d76-4d9b-82aa-06e676988b02', contemporary_threat_escalation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a93565c-9d76-4d9b-82aa-06e676988b02', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, security_hedgers).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nonnuclear_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, disarmament_constituencies).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_absolute).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, treaty_contingency_on_environment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess technical capacity for nuclear weapons and face regional security threats. Benefit from Article X's withdrawal right by maintaining a credible exit option: if regional threat escalates or great-power security guarantees erode, withdrawal becomes feasible. The right is not exercised, but its availability enables strategic ambiguity and deters commitment pressure. Credible exit preserves negotiating position in regional conflicts.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    organized, generational, arbitrage, national).

% Great powers that maintain ambiguity about their own disarmament obligations (Article VI). The withdrawal right's availability reinforces their argument that obligations are contingent on environment; their own capacity to withdraw (or threaten withdrawal) if deterrence needs require it validates this reading for lower-tier states. Exit optionality flows upward from Article X to Article VI interpretation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, security_hedgers, beneficiary,
    institutional, generational, mobile, global).

% The nonlinear dynamic that nonproliferation regimes depend on: proliferation risk declines only when states perceive withdrawal costs exceed staying benefits. When Article X is read as enabling withdrawal for security reasons, the incentive reverses: threshold states gain option value, compliance becomes voluntary rather than binding, and the regime's collective-action foundation weakens. This is a doctrinal victim—not an actor, but a principle whose functional viability the constraint corrodes.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% Forgo nuclear weapons in exchange for nonproliferation assurances and Article VI disarmament commitments from nuclear powers. When withdrawal rights are read as sovereignty-restoring (i.e., reversible commitments), the bargain weakens: nonnuclear states cannot threaten withdrawal in response, but threshold states can. This asymmetry erodes the reciprocity that justifies their sacrifice.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonnuclear_states, payer,
    moderate, generational, constrained, global).

% Civil-society actors, diplomatic coalitions, and treaty-review bodies committed to Article VI as a binding legal obligation with temporal urgency. When withdrawal rights are read as enabling contingent rather than absolute commitments, the grounds for disarmament pressure on nuclear powers erode. The reading undercuts advocacy leverage by reframing Article VI obligations as climate-contingent, not mandatory.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, disarmament_constituencies, payer,
    moderate, biographical, constrained, global).

% Formal parties to the treaty, signatories to Article VI disarmament obligations, and de facto interpreters of treaty meaning through practice and declaration. They maintain the reading that withdrawal is a legitimate state right contingent on security environment, which implicitly reserves their own right to rearm if deterrence conditions warrant. They administer the treaty's enforcement through the NPT Review conferences and influence the threshold-state exit-option assessment through security theater (alliance commitments, deterrence posture).
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Extended-deterrence states (e.g., Japan, South Korea, Middle Eastern treaty partners) that depend on great-power nuclear umbrellas and face regional nuclear threats. They are excluded from the treaty's formal structure but bear costs from the withdrawal-right reading: if threshold states in their region exploit Article X, their security guarantees become contingent, and their own alliance commitment becomes less credible. Their voice would emphasize the binding nature of regional security arrangements, but they have no seat at the NPT.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regional_security_providers, excluded,
    institutional, generational, trapped, regional).

% Official NPT Review Conferences and subsidiary treaty bodies interpret Article X and negotiate consensus language on withdrawal legitimacy. They generate no binding rulings but shape diplomatic norms. Observer role: they witness the contest between readings and produce textual snapshots of which reading is gaining consent at each cycle.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, treaty_review_bodies, observer,
    organized, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a multilateral legal framework preventing horizontal proliferation (spread of nuclear weapons to new states) by binding all signatories to forgo weapons development in exchange for access to peaceful nuclear technology and disarmament commitments from nuclear powers. The framework's stability depends on threshold states finding compliance more attractive than exit.
% TRANSFER_FUNCTION: Moves the burden of nuclear restraint from threshold states (who forgo military capability) to nuclear powers (who commit to disarmament under Article VI). Under this reading, the transfer becomes contingent and reversible: threshold states retain the option to reclaim restraint as the price of continued NPT membership, converting a permanent bargain into a revocable contract.
% ABSENT_VOICES: Non-state armed groups, future generations bearing proliferation risk, and subnational actors in threshold states (who may face coercive pressure if withdrawal threatens their security) have no formal voice. They would argue withdrawal decisions must account for collective harm and generational consequence, but are excluded from treaty negotiation and Review Conferences.
% DISAPPEARANCE_RATIONALE: If this reading were repudiated and Article X reinterpreted as a narrow technical right (emergency exit only, not a sovereignty prerogative), threshold states would immediately face intensified compliance pressure and fewer options for strategic ambiguity. Regional nuclear proliferation would accelerate in unstable zones, and the regime would polarize between states that accept permanent nonproliferation and states that treat it as a contingent choice. Alternatively, if the reading were crystallized into formal doctrine, nuclear-threshold states would shift military planning to assume eventual withdrawal as a strategic option, and the regime would degrade into a delay mechanism rather than a restraint binding.
% FOUNDING_PROBLEM: The NPT was drafted in 1968 to address horizontal proliferation: the danger that as nuclear technology spread, more states would develop weapons, raising the risk of nuclear war, accidents, and arms races. The compromise was binding nonweapon states to forgo weapons in exchange for nuclear technology access and nuclear powers' commitment to disarm. Article X's withdrawal right preserved state sovereignty by allowing exit in extraordinary circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states (particularly Russia and China) invoke the founding problem and cite regional security threats as justification for treating withdrawal as a sovereignty right. Threshold states (North Korea, Iran, and states in unstable regions) operationalize this reading by maintaining withdrawal threats as negotiating leverage. However, disarmament constituencies, nonnuclear states, and many arms-control experts dispute that the founding problem persists as described: they argue the problem has shifted to vertical proliferation (arms racing by states that already have weapons) and verification failure, not new proliferation. The NPT Review Conference language from 2015 and 2022 shows no consensus that the founding problem justifies contingent rather than binding obligations.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.22 (1970, reading incipient) to 0.68 (2026) because the reading's operationalization increases: North Korea's withdrawal threat in 2003, Iran's nuclear program opacity and defiance language, and normalization of 'extraordinary events' language at Review Conferences all instantiate the sovereignty reading as a live negotiating position. Threshold states accrue option value—they can credibly threaten withdrawal to extract concessions (sanctions relief, technology access, security guarantees). Suppression requirement rises from 0.41 to 0.71 because the regime depends on active diplomatic pressure to keep threshold states compliant; as the withdrawal right becomes credible, more institutional effort goes into deterring its invocation (security theater, alliance management, conditional sanctions). Theater ratio rises from 0.08 to 0.42 because the share of enforcement activity devoted to signaling 'withdrawal would be catastrophic' (rather than addressing actual compliance gaps) has grown. The measurement grid is aligned: every metric is authored at every time point, tracking the reading's historical trajectory.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states compute this constraint as legitimate sovereignty preservation (they see exit optionality as their own right too); threshold states compute it as negotiating leverage; nonnuclear states and disarmament advocates compute it as regime decay. The engine computes these divergences from the structural data: the beneficiary seat (threshold states) has high d (targets of extraction are absent from their calculation; they are extractors); the payer seats (regime stability, nonnuclear states) have low or moderate d because they bear costs without exit. The agenda-setter (nuclear powers) maintains the reading's legitimacy through their interpretive authority over the treaty.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states occupy a peculiar structural position: they are listed as beneficiaries (they gain option value, strategic ambiguity, and negotiating leverage from the reading), yet they also appear to pay a cost (they remain under compliance pressure even as their exit becomes credible). This is resolved by their exit_options: they are 'arbitrage'—they can credibly threaten exit to extract concessions, then stay if concessions are sufficient. Their net position is beneficiary: they extract value from the threat while maintaining restraint. Nonnuclear states are straightforward payers: they forgo weapons forever, while threshold states retain optionality. Regime stability is a doctrinal payer: the reading's credibility corrodes the binding nature of nonproliferation commitments. Nuclear weapon states are agenda-setters: they maintain the reading through their diplomatic influence and their own practice of treating Article VI as contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mislabeling by declaring both beneficiaries (threshold states gain negotiating power) and victims (regime stability, nonnuclear states, disarmament advocates lose reciprocity and binding assurances). The 'coordination' aspect is genuine but eroded: the constraint still coordinates horizontal proliferation prevention, but its effectiveness depends on voluntary compliance rather than binding obligation. Active enforcement is required (suppression value 0.71) to maintain the coordination against the reading's tendency to dissolve it. The theater ratio tracks performativity: as the extraction becomes more visible (threshold states invoking withdrawal threats), more institutional energy goes into 'security reassurance' theater rather than actual proliferation prevention, which is the mark of a constraint whose functional core has been partially replaced by self-interested narration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_events_scope,
    'What events qualify as ''extraordinary'' under Article X? Does regional security threat alone suffice, or must it be existential?',
    'Review of historical withdrawal notifications (North Korea 2003, Iran statements 2020–2024) and analysis of whether the invoked circumstances meet consensus thresholds; advisory opinions from international courts or treaty bodies.',
    'If regional threat suffices, withdrawal becomes a routine negotiating tool and extractiveness remains high (0.68+). If only existential threat qualifies, threshold states lose option value and the reading collapses toward a narrower legal interpretation; extractiveness falls to 0.35–0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_events_scope, conceptual, 'Whether extraordinary events threshold is permissive (enabling withdrawal as negotiating leverage) or restrictive (emergency exit only).').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the regime''s coordination function (horizontal proliferation prevention) separable from Article X''s withdrawal right, or does contingency fundamentally undermine the coordination?',
    'Counterfactual: NPT without Article X withdrawal right, or with narrower withdrawal scope. Empirical: examine proliferation dynamics in regimes with and without exit options.',
    'If separable, the reading is extractive but the coordination persists; if inseparable, the reading''s persistence guarantees regime decay and eventual replacement. The theater ratio would then be a trailing indicator of incipient regime collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether nonproliferation coordination can survive contingent-obligation framing.').

omega_variable(
    reciprocity_under_contingency,
    'Can reciprocal bargains (nonnuclear restraint for disarmament progress) hold when obligations are contingent? If threshold states exit when security worsens, do nonnuclear states have grounds to exit when disarmament stalls?',
    'Formal game-theory analysis; historical review of states that abandoned restraint citing lack of reciprocal disarmament; diplomatic statements by nonnuclear-state delegations.',
    'If reciprocity requires binding obligations, the reading undermines the entire bargain: nonnuclear states gain grounds to withdraw, regime collapses into voluntary association. If contingency can be asymmetric (threshold states exit for security, nuclear powers do not exit for disarmament failure), the regime persists as an extraction mechanism with declining legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_under_contingency, conceptual, 'Whether the sovereignty reading is compatible with the reciprocal-bargain structure the treaty claims.').

omega_variable(
    kernel_reading_contest,
    'Is the sovereignty reading a defensible interpretation of Article X''s text, or a post-hoc reframing that privileges exit optionality over binding commitment?',
    'Textual exegesis of Article X''s history of negotiations, statements by drafters, Review Conference consensus language, and legal scholarship; comparison with other treaty withdrawal clauses.',
    'If defensible, the reading is a legitimate contestation and the regime must accommodate multiple interpretations; if a reframing, the reading is a false-sovereign cover for extraction and the regime should crystallize the reciprocal_disarmament_reading as canonical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading is textually grounded or a motivated interpretation serving power interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement_basis(npt__tr_t1970, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement_basis(npt__tr_t2020, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement_basis(npt__be_t2020, observed).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(npt__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.41).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(npt__su_t2020, observed).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(npt__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% The NPT is a contested kernel with three readings. This story instantiates the withdrawal_sovereignty_reading: Article X withdrawal as a legitimate sovereignty right, obligations contingent on security environment. The sibling oligopoly_enforcement_reading frames Articles I-II as the binding primary obligation (horizontal proliferation prevention as an enforcement oligopoly by nuclear powers), with Article VI as contingent. The sibling reciprocal_disarmament_reading frames Article VI as binding with temporal urgency, creating a reciprocal horizontal-vertical bargain. The three readings have different ε values: this one (0.68, substantially extractive) differs from the reciprocal reading (lower ε, emphasizing coordination) and the oligopoly reading (higher ε, emphasizing enforcement). Each reading has distinct beneficiary/victim sets. The readings are linked by network.affects_constraints to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
