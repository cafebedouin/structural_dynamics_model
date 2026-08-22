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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right: Sovereignty Reading
 *   domain: international_law/security
 *
 * SUMMARY:
 *   The Nuclear Nonproliferation Treaty (1970) is a contested international
 *   legal kernel. This constraint story instantiates the
 *   withdrawal_sovereignty_reading: Article X withdrawal right is a
 *   legitimate exercise of sovereign state prerogative, and treaty
 *   obligations are contingent on the security environment remaining stable.
 *   Under this reading, threshold states (Japan, South Korea, Iran, Saudi
 *   Arabia, Turkey, Egypt) gain valuable option value from the ability to
 *   credibly threaten withdrawal, which they can deploy to bargain for
 *   stronger security guarantees or to justify weapons development if
 *   external threats rise. Non-nuclear states and regime institutionalists
 *   (the reciprocal_disarmament_reading) see the same Article X as a
 *   legitimate but minimally exercised safety valve, not a legitimating basis
 *   for conditional compliance. The oligopoly_enforcement_reading treats
 *   Article X as structurally subordinate to Articles I-II's horizontal
 *   proliferation ban and reads withdrawal attempts as regime violations.
 *   These are not different empirical claims about the treaty text — they are
 *   different normative interpretations of what the treaty's legitimacy
 *   *rests on*. This story generates the withdrawal_sovereignty_reading as a
 *   clean, ε-invariant constraint: extractiveness 0.68 (high due to threshold
 *   state option value and regime stability degradation), suppression 0.45
 *   (enforcement machinery is weak — Article X cannot be unilaterally
 *   canceled, only interpreted), theater_ratio 0.22 (some of the enforcement
 *   activity defends the withdrawal right's legitimacy; much of it is genuine
 *   security negotiation). The measurement series tracks a 56-year historical
 *   arc from the treaty's entry (1970, weak extraction as regime was new and
 *   security bargain seemed robust) through Cold War consolidation, post-Cold
 *   War security transitions, to contemporary competition and North Korea's
 *   trajectory, Iran's nuclear program, and Japan/South Korea's wavering NPT
 *   commitment.
 *
 * KEY AGENTS:
 *   - threshold_states: moderate power, identity-locked to NPT status but possessing option value from withdrawal threat; gain strategic flexibility in security negotiations
 *   - established_nuclear_weapons_states: institutional power, agenda-setters for regime interpretation; arbitrage between NPT commitments and strategic interests; gate what counts as 'extraordinary events'
 *   - non_threshold_non_nuclear_states: powerless, trapped in regime; face external security effects if threshold states withdraw and acquire weapons; cannot block withdrawal under this reading
 *   - regime_stability_norm: abstract victim; degraded by credible exit threats that shift treaty from binding to conditional
 *   - security_maximizers: powerful strategic planners across all states; benefit from this reading's licensing of contingent compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right: Sovereignty Reading").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/security").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'ae3b9657-354a-4660-b88e-8f13a6ee9e5d').
narrative_ontology:cs_kernel_codification('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', fixed_text).
narrative_ontology:cs_authority_grounding('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', extraction).
narrative_ontology:cs_interpretation_layer_present('ae3b9657-354a-4660-b88e-8f13a6ee9e5d').
narrative_ontology:cs_reading_relation('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', foundational, withdrawal_as_sovereign_prerogative).
narrative_ontology:cs_axiom_status(withdrawal_as_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', withdrawal_as_sovereign_prerogative, deontological).
narrative_ontology:cs_axiom('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', foundational, treaty_obligations_contingent_on_security).
narrative_ontology:cs_axiom_status(treaty_obligations_contingent_on_security, holdable).
narrative_ontology:cs_axiom_grounding('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', treaty_obligations_contingent_on_security, empirically_contingent).
narrative_ontology:cs_reference_frame('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', binding_permanent_treaty_regime).
narrative_ontology:cs_drift_state('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', contemporary_multi_polar_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae3b9657-354a-4660-b88e-8f13a6ee9e5d', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, security_maximizers).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, npt_institutional_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_threshold_non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with nuclear technical capacity but non-nuclear status under NPT (Japan, South Korea, Iran, Egypt, Saudi Arabia, Turkey). They benefit from the withdrawal right as a credible exit option that preserves their strategic optionality in a shifting security environment. The reading gives them legitimacy to condition continued compliance on external security guarantees (extended deterrence, regional balances). However, they are also locked into NPT membership by their own identity as 'responsible threshold states' — exiting is theoretically costless but practically delegitimizes them. They pay indirectly through continued nonproliferation constraints despite the constant option-value temptation.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, payer).

% The five NNWS (US, UK, France, Russia, China) set the framework for interpreting Article X. Under this reading, they assert the right to withdraw contingent on 'extraordinary events' (security environment shifts). They also claim the right to interpret what constitutes extraordinary events and whether a state's withdrawal is a legitimate exercise of sovereignty or a violation of the regime's spirit. They administer the Review Conferences and gate access to security assurances. They are institutional actors with nearly costless exit (they already possess nuclear weapons) and can arbitrage between their NPT commitments and their strategic interests.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, established_nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% States with no nuclear capacity or technical path to it (most of the Global South, smaller developed nations). They renounce all nuclear weapons and their costs are borne in the form of unequal security. If threshold states exercise the withdrawal right and acquire weapons, non-nuclear states face an arms race security externality with no exit. Their only formal voice in interpreting the treaty is via Review Conference consensus, which they cannot block. The withdrawal reading increases the credibility of exit threats from threshold states, which undermines the regime's core bargain: you give up weapons, we ensure no one else gets them.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_threshold_non_nuclear_states, payer,
    powerless, generational, trapped, global).

% The formal institutional body that meets every five years to assess compliance and interpret the treaty. Functions as a venue where the different readings compete for endorsement. This reading treats the Review Conference as subordinate to the Article X right — withdrawal is a state-level sovereign act not subject to collective veto. The Conference's consensus rule becomes a coordination device rather than a constraint on exit.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, npt_review_conference, observer,
    organized, biographical, constrained, global).

% Strategic planners (defense ministries, intelligence agencies) in states of any nuclear status who prioritize survival over regime participation. This reading gives them a legitimacy frame for treating NPT obligations as contingent on threat assessment. They benefit from the conditionality because it licenses strategic hedging and preserves options.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, security_maximizers, beneficiary,
    powerful, biographical, arbitrage, global).

% NGOs, UN disarmament bodies, and advocacy coalitions committed to the reciprocal_disarmament reading (emphasizing Article VI's binding force). They have advisory status at Review Conferences but no voting power. This reading marginalizes their interpretation by prioritizing Article X sovereignty over Article VI obligation. They are excluded from decision-making about which reading the regime endorses.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, disarmament_advocacy_community, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, established_nuclear_weapons_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a durable global norm against horizontal nuclear proliferation by creating a stable treaty framework that threshold and non-nuclear states believe will hold. The coordination solves a security dilemma: states renounce weapons development in exchange for assurance that others will do the same and that the nuclear five will provide security. The withdrawal right, under this reading, is part of the coordination mechanism — it gives states a legitimate safety valve so that permanent rigidity does not drive defection.
% TRANSFER_FUNCTION: Moves the strategic option value of nuclear weapons development from threshold states to the established nuclear powers, who retain the ability to withdraw and who gate the interpretation of what constitutes 'extraordinary events' justifying withdrawal. Non-nuclear states transfer their security guarantee to a treaty whose permanence is contingent on security environments beyond their control. The constraint transfers stability costs upward: regime stability becomes a public good non-nuclear states must maintain by continued compliance, even when threshold states' exit threats rise.
% ABSENT_VOICES: Non-state actors (NGOs, humanitarian organizations) with stakes in nuclear risk but no seat at treaty negotiations. Disarmament advocacy groups whose reading prioritizes the reciprocal_disarmament_reading are formally excluded from Review Conference voting. Future generations in non-nuclear states whose security environment will be shaped by withdrawal credibility. Scientific bodies that assess non-proliferation verification capability have advisory but not voting roles.
% DISAPPEARANCE_RATIONALE: If the withdrawal sovereignty reading disappeared (i.e., if Article X were reinterpreted as inalienable or if the treaty shifted to explicit permanence), the security rationale for threshold state compliance would degrade dramatically. States like Japan and South Korea would face immediate pressure to reconsider weapons development if their exit option vanished. Regional arms races would accelerate in regions where extended deterrence is contested (Middle East, East Asia). Conversely, if this reading were universally endorsed and formalized, threshold states would openly leverage withdrawal threats in security negotiations, making the nonproliferation regime explicitly conditional — the NPT would become a coalition of the moment rather than a stable order.
% FOUNDING_PROBLEM: The NPT was negotiated in 1968 amid Cold War security competition and emerging nuclear proliferation (China's 1964 test, non-aligned states seeking weapons status). The founding problem was: how do you get most states to renounce nuclear weapons when some possess them and nuclear weapons provide security in an anarchic international system? The solution was a bargain: horizontal proliferation banned, vertical disarmament promised by the five, in exchange for non-nuclear states accepting security subordination. Article X provided a safety valve — states could remain in the treaty while preserving a sovereign exit if the security bargain failed.
% FOUNDING_PROBLEM_CORROBORATION: The US, Russia, and France have all asserted (in Review Conference statements and policy documents) that Article X withdrawal is a legitimate sovereign right when 'extraordinary events' threaten supreme national interests — supporting this reading's claim of the founding problem remaining live. However, non-nuclear states and NGOs argue that the founding problem is no longer what it was: disarmament has not progressed as promised, and making withdrawal more credible actually undermines the bargain it was meant to preserve. Japan's strategic ambivalence (wavering on continued NPT commitment after security shifts) and Iran's withdrawal threat (2018, cited Article X explicitly) both instantiate this reading as a lived constraint. The UNIDIR and UN disarmament bodies present the reciprocal reading, arguing the founding problem is unmet disarmament, not impermanent security commitments.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.35 (1970) to 0.68 (2026) because the credibility of the withdrawal threat increases as security environments become more turbulent (Cold War stability → post-Cold War uncertainty → multi-polar competition with China rise → Russia aggression → climate/migration stress on security frameworks). Each inflection point raises threshold states' perception that Article X is a usable exit, not ceremonial. Suppression stays moderate (0.45 at end) because the NNWS cannot actually prevent withdrawal — the constraint operates through compliance incentives and legitimacy, not coercion. Suppression requirement rises because enforcement must work harder: as threshold states' exit option becomes more credible, the regime must invest more diplomatic effort and security assurance to keep them compliant. Theater_ratio rises slowly (0.08→0.22) because Review Conferences become increasingly performative — they affirm the disarmament obligation while knowing it is unfulfilled, and they reaffirm regime stability while institutional actors are hedging against its collapse. Accessibility_collapse (0.35→0.52 structural level) reflects that alternative arrangements (ad hoc security partnerships, regional nuclear hedging) do exist for threshold states, so the NPT is not a natural law but a contingent institutional choice. Resistance is high throughout (0.58→0.72 structural) because many states and NGOs actively contest this reading — the reciprocal_disarmament and oligopoly readings represent organized resistance to the sovereignty framing.
 *
 * PERSPECTIVAL GAP:
 *   A threshold state (e.g., Japan under pressure from rising China/Russia) reads Article X as legitimating a renegotiation conversation: 'our commitment is contingent on the security guarantee remaining credible.' A non-nuclear state (e.g., Egypt) reads the same text as unfairly conditional: 'we renounced forever, but they renounce only until they feel threatened.' The US reads it as a safety valve that keeps the regime together: 'if we didn't have Article X, threshold states would hedge more aggressively or withdraw secretly.' The UN disarmament community reads it as a loophole in binding disarmament: 'Article VI's promise to disarm is supposed to be permanent, not revocable when security improves.' These are genuinely different constraint perceptions flowing from different power positions and different stakes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (threshold_states, security_maximizers): These agents gain strategic optionality from the withdrawal right. The constraint licenses their reinterpretation of treaty obligations as contingent rather than binding, which is valuable when security environments shift. Their exit is identity_locked, not mobile — exercising withdrawal would delegitimize them, so the threat value is high but exercise cost is high too. The moderate power level limits their capacity to unilaterally reinterpret the regime, but their option value gives them leverage in multilateral negotiations. Directionality: ~0.30-0.45 (beneficiary-ward but not fully subsidized, since exercising the option carries costs). Target directionality (non_nuclear_states, regime_stability_norm, npt_institutional_legitimacy): These bear the extraction cost directly. Non-nuclear states face security externalities if threshold states withdraw (arms race risk, credible proliferation threats). The regime stability norm is damaged by credible exit threats. Institutional legitimacy is revocable rather than intrinsic. Their exit is trapped — they cannot withdraw the withdrawal right itself — and their power is powerless or organized (they can raise concerns in Review Conferences but cannot block consensus). Directionality: ~0.75-0.88 (target-ward: high extraction potential, trapped exit, no exit from the regime without losing legitimacy). Agenda_setter (NNWS) directionality: ~0.35-0.50 (moderate beneficiary, since they preserve the core prohibition on horizontal proliferation while maintaining their own exit option via arbitrage). The asymmetry between the threshold state beneficiary seat and the non-nuclear state target seat is the core structural extract: the constraint preserves strategic optionality for those with capacity while locking those without capacity into permanent obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to get states to renounce weapons when nuclear weapons provide security) remains live in the security-contingency framing that this reading instantiates. However, the mandate (durable regime norm against proliferation) has partially outlived its effectiveness: threshold states now openly leverage Article X as a renegotiation tool (Iran's 2018 withdrawal threat, Japan/SKorea's periodic nuclear hedging debates), and the NNWS's inability to fulfill Article VI's disarmament promise undermines the regime's reciprocal legitimacy. The constraint persists not because the founding problem is solved (it is not) but because both the NNWS and threshold states benefit from a regime that is strict enough to prevent untargeted proliferation but loose enough that option value is preserved. This is a mandatrophic structure: the regime's continued existence depends on its apparent binding force, but its actual operation is conditioned on the threat that binding force is revocable. The theater_ratio (0.22) reflects this: Review Conferences reaffirm permanent obligations while institutional actors behave as if obligations are contingent. The regime persists by maintained ambiguity about whether Article X is a rare escape or a routine negotiating tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_environment_definition,
    'What constitutes an ''extraordinary event'' justifying withdrawal under Article X? Is it objective (adversary nuclear test, invasion, regime change in a neighbor) or subjective (threat perception, strategic environment shift, erosion of security guarantees)?',
    'Comparative cases: review actual withdrawal claims and how the NNWS gate-kept their legitimacy. Track whether states asserting Article X invoked objective events (North Korea''s tests) or subjective threat shifts (Japan/SKorea extended deterrence erosion).',
    'If ''extraordinary events'' is objective, the withdrawal right is rarely exercised and regime stability persists. If subjective, it becomes a standard tool of strategic negotiation and the regime shifts to conditional compliance. This reading assumes substantial subjectivity, which drives the extractiveness measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_environment_definition, conceptual, 'Whether Article X withdrawal grounds in objective or subjective threat assessment.').

omega_variable(
    threshold_state_option_value,
    'How much strategic option value do threshold states actually derive from credible withdrawal threats, relative to the normative cost of exercising them?',
    'Model threshold state decision-making under different assumptions about deterrence strength and security guarantee stability. Compare stated preferences in confidential reviews vs. public positions. Trace whether threats of withdrawal increase during security transitions (e.g., after extended deterrence commitments weaken).',
    'If option value is substantial (security guarantee credibility is low), this reading describes real structural incentives and extraction is genuine. If option value is marginal (extended deterrence is robust), the reading is more theatrical and extraction is lower. Current measurements assume moderate-to-substantial option value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_option_value, empirical, 'Magnitude of strategic optionality threshold states gain from credible withdrawal.').

omega_variable(
    regime_norm_victim_status,
    'Can ''regime stability norm'' legitimately be listed as a victim of this reading, or is naming an abstract norm as victim a category error?',
    'Clarify whether regime norms are structural facts whose degradation harms real agents (non-nuclear states lose security assurance, NNWS lose legitimacy), or whether victimhood requires concrete actors. The reading treats regime stability as an autonomous victim; alternative framings dissolve it into its constituent effects on real stakeholders.',
    'If regime norms can be victims, this reading''s structure is confirmed and the constraint extractively targets institutional legitimacy itself. If only concrete actors can be victims, the norm should be removed and its effects traced to threshold states gaining option value and non-nuclear states bearing security risk. This affects how mandatrophy resolution is framed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_norm_victim_status, conceptual, 'Ontological status of institutional norms as constraint victims.').

omega_variable(
    committer_framing_alternative_readings,
    'This is the withdrawal_sovereignty_reading of the npt_treaty_1970 kernel. Sibling readings (oligopoly_enforcement_reading, reciprocal_disarmament_reading) would instantiate structurally different constraints with different ε values. How does the choice of reading affect the measured extraction and the regime''s legitimacy?',
    'Generate constraint stories for each sibling reading using identical structural data but each reading''s distinct axioms. Compare the resulting ε values, victim sets, and type classifications. Map the divergence to specific interpretive choices (e.g., whether Article VI is binding, whether Article X is revocable).',
    'The three readings should emit materially different ε values for the same treaty — this reading (withdrawal_sovereignty) should show higher extraction (0.68) than the reciprocal reading (which emphasizes binding disarmament and mutual obligation) because it privileges threshold state optionality and regime conditionality. The oligopoly reading should show even higher extraction (enforcement of nuclear monopoly). This is how different committer frames locate different constraints in the same institutional kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_alternative_readings, conceptual, 'Committer-framing under-determination: how reading choice instantiates structurally distinct constraints from the same kernel.').


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
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.11).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement_basis(npt__tr_t2018, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement_basis(npt__be_t1970, observed).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement_basis(npt__be_t2018, observed).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement_basis(npt__su_t1970, observed).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.22).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement_basis(npt__su_t2018, observed).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2026, 0.45).
narrative_ontology:measurement_basis(npt__su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2026
narrative_ontology:measurement(npt__grid_01, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(class), 1970, 0.38).
narrative_ontology:measurement(npt__grid_02, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(class), 2026, 0.5).
narrative_ontology:measurement(npt__grid_03, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(individual), 1970, 0.28).
narrative_ontology:measurement(npt__grid_04, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(individual), 2026, 0.44).
narrative_ontology:measurement(npt__grid_05, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(organizational), 1970, 0.42).
narrative_ontology:measurement(npt__grid_06, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(organizational), 2026, 0.58).
narrative_ontology:measurement(npt__grid_07, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(structural), 1970, 0.35).
narrative_ontology:measurement(npt__grid_08, npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse(structural), 2026, 0.52).
narrative_ontology:measurement(npt__grid_09, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(class), 1970, 0.65).
narrative_ontology:measurement(npt__grid_10, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(class), 2026, 0.74).
narrative_ontology:measurement(npt__grid_11, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(individual), 1970, 0.48).
narrative_ontology:measurement(npt__grid_12, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(individual), 2026, 0.62).
narrative_ontology:measurement(npt__grid_13, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(organizational), 1970, 0.52).
narrative_ontology:measurement(npt__grid_14, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(npt__grid_15, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(structural), 1970, 0.58).
narrative_ontology:measurement(npt__grid_16, npt_treaty_1970__withdrawal_sovereignty_reading, resistance(structural), 2026, 0.72).
narrative_ontology:measurement(npt__grid_17, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(class), 1970, 0.25).
narrative_ontology:measurement(npt__grid_18, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(class), 2026, 0.52).
narrative_ontology:measurement(npt__grid_19, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(individual), 1970, 0.15).
narrative_ontology:measurement(npt__grid_20, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(individual), 2026, 0.38).
narrative_ontology:measurement(npt__grid_21, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(organizational), 1970, 0.28).
narrative_ontology:measurement(npt__grid_22, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(organizational), 2026, 0.55).
narrative_ontology:measurement(npt__grid_23, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(structural), 1970, 0.22).
narrative_ontology:measurement(npt__grid_24, npt_treaty_1970__withdrawal_sovereignty_reading, stakes_inflation(structural), 2026, 0.48).
narrative_ontology:measurement(npt__grid_25, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(class), 1970, 0.14).
narrative_ontology:measurement(npt__grid_26, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(class), 2026, 0.42).
narrative_ontology:measurement(npt__grid_27, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(individual), 1970, 0.08).
narrative_ontology:measurement(npt__grid_28, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(individual), 2026, 0.35).
narrative_ontology:measurement(npt__grid_29, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(organizational), 1970, 0.18).
narrative_ontology:measurement(npt__grid_30, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(organizational), 2026, 0.48).
narrative_ontology:measurement(npt__grid_31, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(structural), 1970, 0.12).
narrative_ontology:measurement(npt__grid_32, npt_treaty_1970__withdrawal_sovereignty_reading, suppression(structural), 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% The NPT kernel instantiates three structurally distinct constraints via different reading acts. All three are linked because the treaty text is shared and changing the interpretation of one (e.g., making Article X more credible) shifts the structural incentives for agents in the other readings. The withdrawal_sovereignty reading emphasizes threshold state option value and regime conditionality (ε=0.68). The oligopoly_enforcement reading emphasizes the NNWS's extraction of a nuclear monopoly via Articles I-II (ε would be higher, ~0.78). The reciprocal_disarmament reading emphasizes the binding reciprocal obligation between horizontal and vertical disarmament (ε would be lower, ~0.45). These are not alternative measurements of one constraint — they are different constraints instantiated by different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
