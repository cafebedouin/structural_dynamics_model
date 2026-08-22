% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Winnability Doctrine: Limited Victory Through Counterforce Targeting
 *   domain: political-military/strategic_studies
 *
 * SUMMARY:
 *   Since the mid-1970s, American (and mirror-image adversary) strategic
 *   policy has been organized around the proposition that nuclear weapons
 *   constrain but do not abolish winnability: that limited victory remains
 *   reachable through accurate counterforce targeting, escalation control,
 *   and damage limitation. The lineage runs from Schlesinger's 1974 targeting
 *   guidance through PD-59, the 1980s modernization wave, and into the
 *   current second nuclear age, where counterforce-style postures persist on
 *   all sides while treaty architecture erodes. This file instantiates ONE
 *   reading of the contested kernel war_winnability_post_1945 — the
 *   countervailing_thinkable reading — and authors epsilon for the standing
 *   arrangement under contest (persistent winnable-war planning under nuclear
 *   constraint) by that reading's own lights. The sibling readings
 *   (deterrence_unthinkable, rhetorical_contraction) are separate constraint
 *   files, not hedges inside this one. KEY AGENTS (by structural
 *   relationship): strategic_planning_establishment — agenda-setter
 *   (institutional/identity_locked) — administers the doctrine and its
 *   planning apparatus; military_industrial_complex — primary beneficiary
 *   (powerful/mobile) — collects the program stream;
 *   extended_deterrence_allies — secondary beneficiary
 *   (institutional/constrained) — receive assurance while hosting targets;
 *   arms_control_regimes — primary victim (institutional/constrained) — their
 *   headroom is consumed; civilian_populations_nuclear_target_sets — victim
 *   (powerless/trapped) — bear unpriced crisis risk; domestic_taxpayers —
 *   victim-fiscal (moderate/constrained); adversary_strategic_establishment —
 *   mirror beneficiary-payer (institutional/identity_locked);
 *   minimal_deterrence_advocates — excluded voice (moderate/constrained);
 *   nuclear_revolution_analysts — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.64).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.5).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.64).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Winnability Doctrine: Limited Victory Through Counterforce Targeting").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "political-military/strategic_studies").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'ac56332e-9457-449a-beb1-4c777ba3ab74').
narrative_ontology:cs_kernel_codification('ac56332e-9457-449a-beb1-4c777ba3ab74', distributed).
narrative_ontology:cs_authority_grounding('ac56332e-9457-449a-beb1-4c777ba3ab74', expertise).
narrative_ontology:cs_interpretation_layer_present('ac56332e-9457-449a-beb1-4c777ba3ab74').
narrative_ontology:cs_reading_relation('ac56332e-9457-449a-beb1-4c777ba3ab74', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('ac56332e-9457-449a-beb1-4c777ba3ab74', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('ac56332e-9457-449a-beb1-4c777ba3ab74', foundational, limited_victory_through_counterforce_remains_achievable).
narrative_ontology:cs_axiom_status(limited_victory_through_counterforce_remains_achievable, holdable).
narrative_ontology:cs_axiom_grounding('ac56332e-9457-449a-beb1-4c777ba3ab74', limited_victory_through_counterforce_remains_achievable, empirically_contingent).
narrative_ontology:cs_axiom('ac56332e-9457-449a-beb1-4c777ba3ab74', secondary, escalation_management_preserves_usable_options).
narrative_ontology:cs_axiom_status(escalation_management_preserves_usable_options, holdable).
narrative_ontology:cs_axiom_grounding('ac56332e-9457-449a-beb1-4c777ba3ab74', escalation_management_preserves_usable_options, instrumental).
narrative_ontology:cs_reference_frame('ac56332e-9457-449a-beb1-4c777ba3ab74', escalation_controlled_options_space).
narrative_ontology:cs_drift_state('ac56332e-9457-449a-beb1-4c777ba3ab74', contemporary_second_nuclear_age, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac56332e-9457-449a-beb1-4c777ba3ab74', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_nuclear_target_sets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, adversary_strategic_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, domestic_taxpayers).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, adversary_strategic_establishment).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, escalation_control_theory).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, damage_limitation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uniformed and civilian planners in the defense secretary's office, the joint staff, and the strategic command who write the targeting plans, run the wargames, and publish the doctrine that keeps limited-nuclear-war scenarios operable. Budgets, promotions, and institutional purpose depend on the planning mission continuing; stepping outside the framework means abandoning the professional identity the war colleges and operations-research tradition built. They decide which target sets, alert postures, and modernization requests reach political leadership.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_planning_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Contractors and their congressional allies who build the delivery systems, reentry vehicles, sensors, and command-and-control upgrades that counterforce planning requires. Revenue follows each new accuracy, penetrability, and survivability program regardless of whether the underlying scenario is ever executed; portfolio diversification gives them room to shift lines if any single program dies.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    powerful, biographical, mobile, national).

% Allied governments, above all in Europe and East Asia, whose security guarantees rest on the credibility of American willingness to use nuclear weapons in controlled, graduated ways. They receive assurance from the doctrine's continued existence while hosting forward bases and sitting inside the adversary's target sets; their exits run through acquiring independent arsenals or accommodating the adversary, both costly and politically fraught.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_allies, beneficiary,
    institutional, generational, constrained, regional).

% Treaty negotiators, verification agencies, and the agreement architecture itself — SALT, INF, START, New START — whose ceilings and counting rules are repeatedly overtaken by counterforce deployments justified through winnable-war scenarios. Each modernization round consumes negotiating headroom and narrows what future agreements can cover; the people staffing these regimes cannot leave the process without ceding the field entirely to its opponents.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    institutional, generational, constrained, global).

% Urban and industrial populations on all sides who sit in the combined counterforce-and-countervalue target base and absorb the added risk that accurate, prompt counterforce postures create through use-or-lose pressures in a crisis. They hold no seat in the planning process and cannot relocate out of the target sets.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_nuclear_target_sets, payer,
    powerless, generational, trapped, continental).

% Households funding the strategic modernization bills — bombers, submarines, missiles, warhead life-extension — through appropriations shaped largely inside the planning establishment's request cycle. Their recourse is episodic electoral attention and occasional mass mobilization such as the nuclear freeze campaign of the early 1980s.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, domestic_taxpayers, payer,
    moderate, biographical, constrained, national).

% The opposing power's rocket forces and general staff, who cite the other side's counterforce buildups to justify mirror-image programs, budgets, and doctrine. They gain mission continuity from the action-reaction cycle while their own forces become the target set the opposing doctrine is built against.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, adversary_strategic_establishment, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, adversary_strategic_establishment, payer).

% Analysts and former officials arguing for small arsenals, no-first-use pledges, and arms-control-first posture who are kept outside the planning rooms; their proposals surface in op-eds, hearings, and advisory letters but not in target planning. Present inside, they would dismantle the scenario library the establishment maintains.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, minimal_deterrence_advocates, excluded,
    moderate, biographical, constrained, global).

% Academic scholars of the nuclear revolution who study the gap between what the operational plans assume about escalation control and what the historical and technical record supports. They publish, testify, and train some of the participants, but hold no planning authority and no stake in the program stream.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_revolution_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared operational framework for how nuclear forces would be employed if deterrence fails — target sets, escalation-control procedures, damage-limitation objectives — and gives extended-deterrence assurances to allies a concrete, inspectable operational basis rather than a bare declaratory promise.
% TRANSFER_FUNCTION: Moves fiscal resources from national treasuries and taxpayers to strategic forces and their contractors; moves decision latitude from treaty-negotiation channels into military planning channels; and moves unpriced crisis risk onto the civilian populations embedded in the resulting target sets.
% ABSENT_VOICES: Minimal-deterrence and abolitionist analysts, verification specialists who watch counterforce deployments consume treaty headroom, and the targeted publics themselves are absent from the planning conversation. Present, they would attack the damage-limitation assumptions and the assurance rationale before funds and alert postures moved.
% DISAPPEARANCE_RATIONALE: If the winnability framework vanished overnight, counterforce-specific systems would be stranded without a mission rationale, strategic budgets would contract toward minimum-deterrence force levels, allies would have to renegotiate assurance or hedge independently, and bargaining space that closed around deployed forces would reopen; the adversary's mirror programs would lose their cited justification, and the action-reaction cycle would lose one of its engines.
% FOUNDING_PROBLEM: After 1945, and acutely once both superpowers held survivable thermonuclear arsenals, the inherited problem of how to fight and prevail in a great-power war appeared to dissolve into mutual annihilation. The countervailing tradition formed to restore usable, limited options: how to wage and win a confined nuclear conflict rather than preside over national suicide.
% FOUNDING_PROBLEM_CORROBORATION: That the problem was once treated as live is corroborated from outside the current beneficiary set by declassified planning documents (NSC-68, the Single Integrated Operational Plan reviews, PD-59 itself) and by former officials writing after leaving office. But no source outside the planning establishment and its industrial clients currently attests that the problem remains live today: contemporary liveness claims come almost entirely from within, while nuclear-revolution scholars and veteran arms controllers attest from outside that the problem was either dissolved by unthinkability or never coherent. That corroboration asymmetry is itself the finding.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64: the arrangement systematically moves fiscal resources and treaty headroom from taxpayers and arms-control processes into the counterforce program stream, and imposes unpriced crisis-instability risk on populations with no seat — while a real remainder of its operation (employment planning, assurance substance) serves participants. Suppression is 0.50: no physical coercion operates, but alternatives (minimum deterrence, no-first-use, arms-control-first posture) are held down by institutional gatekeeping, career discipline, and budget exclusion, and the enforcement effort visibly oscillates — high during the freeze-movement fights of the early 1980s, decaying through the post-Cold-War lull, rebuilding as the taboo hardens and open advocacy grows costlier. Theater_ratio is 0.48: wargames that reliably validate the plan, damage-limitation studies resting on heroic input assumptions, and declaratory silence coexist with genuine engineering and targeting content. Accessibility_collapse is 0.35 — alternatives remain articulable and were briefly ascendant in the early 1990s, so understanding the arrangement does not close its exits. Resistance is 0.52 — mass movements, scientific and religious critique, allied hedging, and treaty defenders have repeatedly contested it. The claimed type (tangled_rope) is asserted from structure — a real assurance-and-planning coordination function PLUS systematic asymmetric transfer PLUS active enforcement — independently of these metric values; the engine computes per-seat types from the structural data. The measurement series run on one shared time grid (1974–2024, seven points, every tracked metric authored at every point). The trajectories are cyclical rather than monotonic: rise through the 1980s buildup, decay through the 1990s arms-control flowering, renewed rise with multipolar counterforce competition — driven by the external geopolitical tempo, not by intermittent reinforcement internal to the arrangement; base_properties reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the arms-control and civilian seats the arrangement operates as enforced transfer with no available exit: their treaty headroom and their safety are consumed by decisions made in rooms they cannot enter. From the industry and allied seats the same structure is indispensable coordination — assurance that holds an alliance together and employment planning that would be reckless to lack. From the planning establishment's seat it is vocation: the framework constitutes professional identity, so its perpetuation is experienced as responsibility rather than gain. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the military-industrial complex sits near the beneficiary pole amplified by mobile exit (arbitrage-grade portfolio mobility), while extended_deterrence_allies sit somewhat less low because they simultaneously host the target sets their assurance rides on. Victims derive high directionality: arms_control_regimes are constrained-exit institutional payers; civilian_populations_nuclear_target_sets are trapped and powerless, placing them nearest the full-target end; domestic_taxpayers are moderately placed. The adversary_strategic_establishment is the one genuinely dual-positioned seat: its beneficiary declaration pulls its derived d downward, but it also pays through mirror-program costs and target exposure. I leave it to the structural derivation rather than issuing a directionality override, because its relationship to THIS arrangement is dominated by the feeding side of the action-reaction cycle; the ambiguity is documented in the commentary rather than papered over with an override. No other seat needs an override: the beneficiary/victim declarations plus exit options already place every agent correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restoring usable, limited war options under the nuclear shadow — has a contested status: the unthinkability school holds it was dissolved by the nature of the weapons, the countervailing school holds it live whenever deterrence might fail. Because the status is contested rather than dead, no clean mandatrophy resolution is available; the honest state is a live dispute over obsolescence. The temporal series nonetheless shows the symptom trajectory of partial mandate atrophy: theater_ratio climbs from 0.30 toward 0.48 as damage-limitation claims outrun their evidentiary base, and suppression_requirement rebuilds as the doctrine's public defensibility erodes even while its operational grip holds. Classifying this as tangled_rope rather than snare keeps the genuine assurance function visible — preventing the arrangement from being mislabeled as pure extraction — while the victim declarations keep the transfer visible, preventing the reverse error of reading budget capture as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates one reading (countervailing_thinkable) of the kernel war_winnability_post_1945; what structurally different constraints do the sibling readings instantiate over the same referent?',
    'Author and classify the sibling files separately, then compare: identical referent (persistent post-1945 winnable-war planning), reading-indexed epsilon per OQ-26.',
    'The deterrence_unthinkable reading authors epsilon near zero for the planning activity itself (incoherent labor sustained by inertia) and pushes the arrangement toward theatrical-inertial readings; the rhetorical_contraction reading splits the referent into a discursive layer (speech suppressed, taboos enforced) and an operational layer (unchanged), producing different victim sets — public discourse and dissenters rather than treaty regimes. Classification of THIS file must not be averaged across those outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the post-1945 winnability kernel; sibling readings are separate constraints.').

omega_variable(
    damage_limitation_feasibility,
    'Can counterforce strikes actually limit damage against survivable retaliatory forces — hardened silos, decoys, road-mobile missiles, ballistic-missile submarines — or does the feasibility premise fail on the technical merits?',
    'Independent exchange modeling and historical analysis of target-coverage claims against adversary survivability investments, using declassified weapon-effect and order-of-battle data.',
    'If infeasible, the doctrine''s coordination contribution collapses and the arrangement reads as budgetary self-perpetuation riding a real assurance legacy — shifting the computed classification toward pure extraction; if feasible in bounded scenarios, the coordination function is genuine and the hybrid reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(damage_limitation_feasibility, empirical, 'Whether the doctrine''s core empirical premise (limitable damage via counterforce) survives technical scrutiny.').

omega_variable(
    assurance_vs_budget_sustainment,
    'Is the doctrine maintained because allied assurance genuinely requires visible warfighting options, or because the planning establishment and its industrial base require the mission?',
    'Counterfactual budget analysis: would counterforce programs persist if assurance were deliverable by other instruments (survivability demonstrations, declaratory policy alone)? Compare periods where allied assurance demand was constant while domestic budget politics shifted.',
    'Assurance-driven persistence weights the arrangement toward its coordination half; budget-driven persistence sharpens the victim declarations and the transfer reading, tightening the hybrid toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assurance_vs_budget_sustainment, conceptual, 'Which sustaining cause dominates: allied assurance demand or mission-continuity interests.').

omega_variable(
    crisis_instability_risk_bearing,
    'Who bears the added crisis risk that prompt counterforce postures create through use-or-lose pressure, and is that risk priced anywhere in the arrangement''s own accounting?',
    'Reconstruction of crisis cases (e.g., the 1983 exercise scare) tracing decision-time pressure, combined with a normative analysis of risk imposition on unrepresented populations.',
    'If the risk is unpriced and borne by unrepresented publics, the transfer function includes an uncompensated imposition that reinforces the victim declarations; if it is priced through civilian review or treaty constraint, the extraction picture softens materially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_instability_risk_bearing, preference, 'Distribution and pricing of the crisis-instability externality the doctrine generates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.3).
narrative_ontology:measurement_basis(war__tr_t1974, observed).
narrative_ontology:measurement(war__tr_t1983, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1983, 0.41).
narrative_ontology:measurement_basis(war__tr_t1983, observed).
narrative_ontology:measurement(war__tr_t1992, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1992, 0.37).
narrative_ontology:measurement_basis(war__tr_t1992, observed).
narrative_ontology:measurement(war__tr_t2001, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2001, 0.35).
narrative_ontology:measurement_basis(war__tr_t2001, observed).
narrative_ontology:measurement(war__tr_t2010, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2010, 0.39).
narrative_ontology:measurement_basis(war__tr_t2010, observed).
narrative_ontology:measurement(war__tr_t2019, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2019, 0.44).
narrative_ontology:measurement_basis(war__tr_t2019, observed).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(war__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.44).
narrative_ontology:measurement_basis(war__be_t1974, observed).
narrative_ontology:measurement(war__be_t1983, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1983, 0.6).
narrative_ontology:measurement_basis(war__be_t1983, observed).
narrative_ontology:measurement(war__be_t1992, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement_basis(war__be_t1992, observed).
narrative_ontology:measurement(war__be_t2001, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2001, 0.47).
narrative_ontology:measurement_basis(war__be_t2001, observed).
narrative_ontology:measurement(war__be_t2010, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement_basis(war__be_t2010, observed).
narrative_ontology:measurement(war__be_t2019, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement_basis(war__be_t2019, observed).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement_basis(war__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.4).
narrative_ontology:measurement_basis(war__su_t1974, observed).
narrative_ontology:measurement(war__su_t1983, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1983, 0.58).
narrative_ontology:measurement_basis(war__su_t1983, observed).
narrative_ontology:measurement(war__su_t1992, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1992, 0.34).
narrative_ontology:measurement_basis(war__su_t1992, observed).
narrative_ontology:measurement(war__su_t2001, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement_basis(war__su_t2001, observed).
narrative_ontology:measurement(war__su_t2010, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement_basis(war__su_t2010, observed).
narrative_ontology:measurement(war__su_t2019, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement_basis(war__su_t2019, observed).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.5).
narrative_ontology:measurement_basis(war__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_arms_control_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'the nuclear winnability debate' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one referent: this countervailing reading (hybrid coordination/extraction, actively enforced, epsilon 0.64), the deterrence_unthinkable reading (epsilon near zero for the planning activity itself; the arrangement as inertial labor), and the rhetorical_contraction reading (discursive suppression layered over unchanged operations, with dissenters as victims). Family edges run from this file to both siblings: the countervailing reading's operational persistence supplies the substrate the contraction reading describes and the legitimacy pressure the unthinkability reading answers. The downstream nuclear_arms_control_regime edge records that counterforce deployments structurally consume treaty headroom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
