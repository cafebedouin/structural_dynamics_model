% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty -- Responsibility-to-Protect Forfeiture Regime
 *   domain: international law/political theory/state systems
 *
 * SUMMARY:
 *   Since 2001 the international system has operated a revision of
 *   Westphalian sovereignty: territorial inviolability is treated as a
 *   conditional status that a state forfeits when it fails to protect its own
 *   population from mass atrocity -- genocide, ethnic cleansing, crimes
 *   against humanity. Authorization concentrates in the Security Council's
 *   veto holders; execution falls to ad hoc coalitions; a monitoring and
 *   reporting apparatus grew up around the doctrine. The arrangement solves a
 *   genuine collective-action problem that the older categorical norm left
 *   wide open, and simultaneously concentrates adjudicative rents in the five
 *   veto seats and strategic gains in the intervening coalitions, while
 *   target-population seats and small unaligned states absorb the costs. KEY
 *   AGENTS (by structural relationship): - permanent_five_members:
 *   Agenda-setting seat ([institutional]/[arbitrage]) -- adjudicate the
 *   forfeiture condition, collect gatekeeping rents, insulated by the veto -
 *   humanitarian_intervention_coalitions: Operating beneficiary
 *   ([powerful]/[mobile]) -- receive mandates, legitimacy cover, strategic
 *   access; participation discretionary per crisis -
 *   global_prevention_apparatus: Institutional beneficiary
 *   ([institutional]/[identity_locked]) -- monitoring offices whose existence
 *   presupposes the framework - populations_under_atrocity_regimes: Intended
 *   protectee seat ([powerless]/[trapped]) -- receive protection and absorb
 *   its collateral costs; dual beneficiary/payer - target_regime_leaderships:
 *   Direct payer ([powerful]/[trapped]) -- sanctions, indictment, forcible
 *   removal; exits closed by design - small_unaligned_states: Diffuse payer
 *   class ([moderate]/[constrained]) -- bear generalized inviolability
 *   erosion; present in debate, absent from adjudication -
 *   international_legal_community: Analytical observer
 *   ([institutional]/[analytical]) -- tracks codified-kernel-versus-practice
 *   drift
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.64).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.74).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.64).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty -- Responsibility-to-Protect Forfeiture Regime").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international law/political theory/state systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'f48e4739-c66c-43cd-8006-212e5413a160').
narrative_ontology:cs_kernel_codification('f48e4739-c66c-43cd-8006-212e5413a160', fixed_text).
narrative_ontology:cs_authority_grounding('f48e4739-c66c-43cd-8006-212e5413a160', lineage).
narrative_ontology:cs_interpretation_layer_present('f48e4739-c66c-43cd-8006-212e5413a160').
narrative_ontology:cs_reading_relation('f48e4739-c66c-43cd-8006-212e5413a160', westphalia_sovereignty__westphalia_absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('f48e4739-c66c-43cd-8006-212e5413a160', westphalia_sovereignty__westphalia_graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('f48e4739-c66c-43cd-8006-212e5413a160', foundational, inviolability_conditioned_on_protection).
narrative_ontology:cs_axiom_status(inviolability_conditioned_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('f48e4739-c66c-43cd-8006-212e5413a160', inviolability_conditioned_on_protection, deontological).
narrative_ontology:cs_axiom('f48e4739-c66c-43cd-8006-212e5413a160', secondary, international_adjudication_of_forfeiture).
narrative_ontology:cs_axiom_status(international_adjudication_of_forfeiture, holdable).
narrative_ontology:cs_axiom_grounding('f48e4739-c66c-43cd-8006-212e5413a160', international_adjudication_of_forfeiture, conventional).
narrative_ontology:cs_reference_frame('f48e4739-c66c-43cd-8006-212e5413a160', protective_duty_compact).
narrative_ontology:cs_drift_state('f48e4739-c66c-43cd-8006-212e5413a160', post_libya_backlash_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f48e4739-c66c-43cd-8006-212e5413a160', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, permanent_five_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_prevention_apparatus).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, target_regime_leaderships).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, small_unaligned_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the veto over every authorization of force or sanction issued under the forfeiture condition. They judge when a state has failed its protective duty, shield client states from application, and collect gatekeeping influence: every invocation of the doctrine worldwide passes through their assent. Because they wrote and maintain the rules, they can block any adverse application against themselves or their partners; their exposure to the conditionality they administer is minimal.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Ad hoc coalitions (NATO in 2011, ECOWAS in 2017) receive mandates, legitimacy cover, basing access, and post-operation leverage when the condition is judged met. Participation is chosen crisis by crisis: states opt in where strategic interest aligns and abstain elsewhere, absorbing treasure and casualty costs but typically recovering strategic positioning. Withdrawal from a formed coalition is costly, but formation itself is discretionary.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, mobile, continental).

% Monitoring and early-warning offices, commissions of inquiry, and special-adviser posts whose mandates, budgets, and careers exist only because the conditional framework exists. They compile atrocity-risk assessments and report shortfalls, but hold no enforcement power of their own. The institutions' self-concept is bound up with maintaining the framework: their annual reporting cycle presupposes it, and dismantling it would dissolve their reason for being.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_prevention_apparatus, beneficiary,
    institutional, generational, identity_locked, global).

% The people the doctrine exists to protect. When enforcement fires they receive protection from atrocity; they also absorb the air campaigns, sanctions hardship, infrastructure destruction, and post-intervention disorder that accompany it, and they live under the precedent that their government's conduct licenses foreign force deployed over their heads. They cannot leave the territory being fought over, and their voice in the authorization decision is zero; they are spoken for by others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer).

% Governments judged to have failed their protective duty face asset freezes, travel bans, indictment referral, and ultimately forcible removal. The enforcement design deliberately closes their exits: no guaranteed asylum, no safe passage, no negotiated immunity from the instruments aimed at them. Their survival, liberty, and patrimony ride directly on how the conditionality is applied.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, target_regime_leaderships, payer,
    powerful, biographical, trapped, national).

% States without veto seats, nuclear deterrents, or great-power patrons carry the generalized erosion of the inviolability guarantee: enforcement statistically selects against states shaped like them, so their sovereignty claims trade at a discount regardless of their own conduct. They caucus and litigate in assembly corridors and issue counter-declarations, but hold no seat in the adjudicative body that decides applications. They cannot rapidly acquire great-power status; their exit from exposure is slow structural change, not maneuver.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, small_unaligned_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, small_unaligned_states, excluded).

% Jurists, tribunal benches, and academic international lawyers who adjudicate the legality disputes the doctrine generates and track the widening distance between the codified texts and operating state practice. They collect no rents from the arrangement and bear none of its costs; their stake is doctrinal coherence.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, permanent_five_members).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem the categorical inviolability norm left open: when the territorial state is itself the perpetrator of mass atrocity, or is unwilling or unable to stop it, no outside actor has legitimate standing or pooled capability to halt the killing. The conditional reading pools legitimacy (authorized action replaces unilateral vigilantism), military capability, and adjudication, converting sovereignty from an unconditional shield into a contract conditioned on protective performance.
% TRANSFER_FUNCTION: Moves adjudicative authority over territorial inviolability from each individual state to the Security Council's veto holders; moves the intervention prerogative from target-state consent to authorized-coalition discretion when forfeiture is judged met; moves material and reputational gains to participating coalitions; moves protection, unevenly and at collateral cost, to at-risk populations; moves legal and existential risk onto target leaderships.
% ABSENT_VOICES: Populations of states struck under contested or unauthorized humanitarian framings had no seat in any deliberation. Global South jurists challenging enforcement selectivity are heard in assembly corridors but absent from the veto-wielding adjudicative chair. Target-state civil societies find their voice displaced by external spokesmen claiming to act on their behalf. The categorical-inviolability tradition's defenders hold debating presence but no decision authority anywhere in the enforcement chain.
% DISAPPEARANCE_RATIONALE: If the conditional framework vanished overnight, the post-2005 architecture would unravel: prevention mandates lapse, regional arrangements modeled on it lose their template, atrocity response reverts to ad hoc coalitions-of-the-willing or outright paralysis, and the default equilibrium reverts to inviolability-as-absolute-shield. Veto holders would lose a legitimizing instrument they actively use; coalitions would lose their authorization channel; at-risk populations would lose the only standing mechanism that has ever overridden their government's claim to non-interference. Arrangements demonstrably depend on it.
% FOUNDING_PROBLEM: The 1990s authorization gap. Rwanda in 1994 and Srebrenica in 1995 showed the absolute inviolability norm shielding industrial-scale killing; Kosovo in 1999 showed unauthorized intervention producing its own legitimacy crisis. The doctrine was built to close the gap: authorize protection of populations without licensing arbitrary invasion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: survivor and memorial institutions at Kigali and Srebrenica-Potocari attest the failure the doctrine answered; the African Union's Constitutive Act Article 4(h), adopted by a non-Western body with no permanent-member seat, embeds the same non-indifference principle; successive Secretary-General implementation reports, produced by an office the doctrine created, document continuing atrocity cases the machinery failed to answer; and scholarship across Global South academies agrees the underlying problem is live even while disputing whether this arrangement serves it.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64 at interval end) is substantial but short of pure-rent levels: the arrangement has genuinely delivered protective coordination in some cases (Kenya 2008 mediation, Cote d'Ivoire 2011, ECOWAS in Gambia 2017) while the same authorization channel has produced mandate-stretch (Libya 2011) and selective invocation whose strategic gains accrue to the authorizing and intervening seats. Suppression (0.74) is the RAW structural coercive content -- veto-gated monopoly on lawful force, the sanctions-and-indictment architecture, deliberate closure of target-leadership exits -- and is authored unscaled; only extractiveness is scaled downstream by directionality and scope. Theater (0.47) rose sharply after 2011: as enforcement capacity decayed under permanent-member discord, the declaratory apparatus (summits, annual thematic reports, commemorative framing) persisted and grew relative to functioning protection -- the signature of a coordination core acquiring performative residue. Accessibility collapse is low (0.45) because rival doctrinal frameworks remain fully live and citable alternatives in legal discourse; resistance is correspondingly high and durable (0.6), renewing after every conspicuous application. All three temporal series run on ONE shared eight-point grid spanning 2001-2025. The trajectory is CYCLICAL, not monotonic: doctrinal build-up (2001-2008), maximal assertion (2011), backlash retrenchment (2014-2017), partial revival (2020-2025). The cycle's driver: each conspicuous application generates sovereignty-bloc counter-mobilization that raises the enforcement price of the next application, so the doctrine alternates assertion and retreat -- and the intermittent assertion is itself partly an extraction mechanism, since each revival re-legitimates coalition prerogatives and adjudicative rents while enforcement stays selective. Base properties are authored at the interval end state (2025), on the ascending edge of the current cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the permanent-member seat the arrangement is responsible guardianship that they administer and can suspend at will. From target-regime leaderships it is licensed aggression with the exits welded shut. From populations under atrocity regimes it is double-edged: the same mechanism billed to protect them also bombs, besieges, and strips away the inviolability recourse their government nominally owed them. From small unaligned states it is selective discipline -- enforcement statistics select against states without patrons or deterrents, and those states hold debating presence but no adjudicative seat. The legal community watches the widening gap between codified kernel and operating practice while collecting and paying nothing. Coalition potential for the powerless seat runs thin: diaspora advocacy and victim-memory institutions exert moral pressure but carry no veto leverage, which is why their trap is structural rather than organizational.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. permanent_five_members and global_prevention_apparatus sit at the beneficiary pole: the veto insulates the administrators from the conditionality they impose, and the apparatus's mandates are products of the framework. humanitarian_intervention_coalitions sit low-to-moderate: mandate gains minus real operational costs. target_regime_leaderships sit nearest the full-target pole because their exit is deliberately closed; small_unaligned_states sit slightly inside them, retaining constrained-but-real diplomatic exit. One directionality override is authored: populations_under_atrocity_regimes derive a deep-beneficiary d from their beneficiary listing, which understates their position -- the same mechanism meant to subsidize them bills them through collateral destruction and precedent erosion, so their d is pinned to 0.45 (near-symmetric, beneficiary-leaning). The override is keyed to the powerless atom because that atom maps uniquely to this seat in this story. No other overrides were needed: the derivation separates the two powerful seats correctly because it reads the beneficiary/victim lists rather than power atoms alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- the 1990s authorization gap -- is live, not dead: mass atrocity persists and the machinery stalled in most recent cases, so this is not mandatrophy and no sunset logic applies. The mislabeling hazards run both ways. Calling the arrangement a snare erases the real protective coordination (Kenya, Cote d'Ivoire, Gambia) that the categorical norm never delivered; calling it a rope erases the adjudicative capture and coalition rents layered on top of that coordination. The tangled-rope claim keeps both halves visible and lets the engine weigh them. The forward hazard is piton drift: if enforcement capacity continues eroding while the declaratory apparatus persists, theater_ratio climbs further and the arrangement converges on performed guardianship -- the 2014 theater inflection is the leading indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the conditional_responsibility reading the operative framework governing a given crisis, or do the sibling readings (absolute_non_intervention, graded_sovereignty) govern instead?',
    'Crisis-by-crisis coding of which framework actors actually cite when inviolability is contested: Charter Article 2(7) invocations (categorical reading), World Summit Outcome paragraph citations (this reading), capacity and statehood assessments (graded reading), together with veto explanations and assembly debate records.',
    'This story authors epsilon for the conditional reading only, over the standing conditional-sovereignty arrangement. Under the categorical sibling the adjudicative extraction disappears but atrocity-shielding extraction appears; under the graded sibling the victim set expands to all low-capacity states. Classification and victim structure shift wholesale with whichever reading governs -- the readings are separate constraints, not measurement settings on this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame locator: this constraint is one reading of the westphalia_sovereignty kernel; the sibling readings are separate constraint stories.').

omega_variable(
    forfeiture_threshold_evidence_standard,
    'What evidentiary standard determines that a state has failed to protect its population severely enough to forfeit inviolability, and who accepts which evidence?',
    'Comparative case analysis of authorized interventions (Libya 2011, Cote d''Ivoire 2011, Gambia 2017) against unauthorized atrocity cases (Syria, Myanmar, northern Ethiopia): reconstruct the threshold evidence present in each and trace which bodies accepted or rejected it.',
    'A wide discretionary band makes the conditionality clause a licensable instrument and pushes effective extraction toward the snare boundary; a tightly evidence-bound threshold restores the coordination-dominant profile and shrinks the effective victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_threshold_evidence_standard, empirical, 'Operational ambiguity of the mass-atrocity trigger that defines the entire victim set.').

omega_variable(
    adjudication_capture_by_permanent_members,
    'Is the adjudicative authority this reading vests in the international community exercised collectively, or captured by the five veto holders'' bilateral interests?',
    'Code every forfeiture determination from 2005 onward by whether the prospective target state held a permanent-member patron; measure the correlation of authorization outcomes with patron absence.',
    'High capture converts the tangled-rope profile toward snare -- extraction wearing coordination as cover -- while low capture supports the genuine-collective-guarantor reading and lowers effective extraction at the administered seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_capture_by_permanent_members, empirical, 'Whether the adjudicative seat the reading created functions as collective organ or private gate.').

omega_variable(
    enforcement_selectivity_direction,
    'Does application of the forfeiture condition track atrocity severity or the target state''s power position?',
    'Cross a severity-ranked atrocity dataset with authorization outcomes; regress intervention probability on severity measures against target-capability measures.',
    'Severity-tracking substantiates the coordination half of the tangled rope; capability-tracking confirms positional extraction borne disproportionately by small_unaligned_states and raises their effective extraction further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_direction, empirical, 'The selectivity gradient that determines whose inviolability is actually conditional.').

omega_variable(
    protection_gain_separability,
    'Can population protection be structurally separated from the strategic gains intervention coalitions collect through the same authorization?',
    'Counterfactual mandate analysis of authorized operations: did the protection objective require the mandate as executed (the Libya mandate-stretch test), and did coalition strategic positions measurably improve through protection-framed operations?',
    'If separable, the intervention-prerogative component is extraction riding on protective coordination, and the epsilon attributable to protection alone drops sharply. If inseparable, part of the measured extraction is the irreducible price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_gain_separability, conceptual, 'Separability of the protective coordination function from coalition strategic extraction within one authorization channel.').

omega_variable(
    constructed_naturalness_ambiguity,
    'Is conditional sovereignty an emergent feature of evolving international order, or a constructed instrument whose principal designers and operators benefit identifiably?',
    'Genealogy of the doctrine from commission reports and summit drafting records, cross-checked against which delegations secured carve-outs and which institutions gained new mandates at adoption.',
    'A constructed pedigree with identifiable beneficiaries supports the tangled-rope classification as authored; a demonstrated emergent-consensus character with diffuse authorship would push the profile toward rope and lower the adjudicative-seat extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_naturalness_ambiguity, conceptual, 'Natural-law-evolution framing versus constructed-instrument framing of the conditionality principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_cond_resp_tr_t2001, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(ws_cond_resp_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(ws_cond_resp_tr_t2008, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(ws_cond_resp_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(ws_cond_resp_tr_t2014, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2014, 0.52).
narrative_ontology:measurement(ws_cond_resp_tr_t2017, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2017, 0.55).
narrative_ontology:measurement(ws_cond_resp_tr_t2020, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2020, 0.5).
narrative_ontology:measurement(ws_cond_resp_tr_t2025, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2025, 0.47).

% Extraction over time
narrative_ontology:measurement(ws_cond_resp_be_t2001, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(ws_cond_resp_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(ws_cond_resp_be_t2008, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(ws_cond_resp_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.68).
narrative_ontology:measurement(ws_cond_resp_be_t2014, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2014, 0.66).
narrative_ontology:measurement(ws_cond_resp_be_t2017, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(ws_cond_resp_be_t2020, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(ws_cond_resp_be_t2025, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(ws_cond_resp_su_t2001, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(ws_cond_resp_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(ws_cond_resp_su_t2008, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(ws_cond_resp_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(ws_cond_resp_su_t2014, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2014, 0.75).
narrative_ontology:measurement(ws_cond_resp_su_t2017, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(ws_cond_resp_su_t2020, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(ws_cond_resp_su_t2025, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_graded_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'Westphalian sovereignty' decomposes into three structurally distinct constraints -- three readings of one kernel. absolute_non_intervention (categorical shield): extraction accrues to atrocity-committing regimes sheltering behind inviolability. conditional_responsibility (this file): adjudicative extraction accrues to veto holders and intervention coalitions atop genuine protective coordination. graded_sovereignty: positional extraction accrues to whoever performs capacity assessment. Each reading has its own epsilon and victim set. They are linked because each is cited as the corrective to the others' failures: the categorical reading's Rwandan failure is this reading's founding evidence, and this reading's Libyan excess is the graded reading's recruiting argument.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
