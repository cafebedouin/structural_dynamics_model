% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Post-1945 Winnability Taboo: Discursive Contraction with Operational Continuity
 *   domain: strategic studies/nuclear deterrence/international relations
 *
 * SUMMARY:
 *   After 1945, the public articulation of nuclear war as winnable collapsed
 *   into stigma — the Herman Kahn episode is the emblem: a strategist who
 *   wrote plainly about thermonuclear exchange conditions was permanently
 *   recast as a warmonger — while the operational layer did the opposite,
 *   elaborating continuously from early target lists through the SIOP's
 *   evolution into limited-options annexes and counterforce refinements. The
 *   sayable contracted; the plannable did not. This story authors ONE reading
 *   of the contested kernel war_winnability_post_1945: the
 *   rhetorical_contraction reading, in which the discursive space around
 *   winnability narrowed while the operational space persisted behind
 *   classification, producing a standing gap between what democracies say
 *   about these weapons and what their planners prepare them to do.
 *   Beneficiaries: strategic planners, who retain operational flexibility
 *   without public adjudication of their premises, and the executive
 *   leadership that controls both the declaratory line and the classification
 *   wall. Victims: legislative overseers, formally empowered but structurally
 *   unable to audit premises they cannot quote, and the electorates who
 *   underwrite and bear the risks of plans they cannot examine. Per the
 *   epsilon-invariance principle this kernel decomposes into three constraint
 *   stories — deterrence_unthinkable, countervailing_thinkable, and this one
 *   — each with its own epsilon, beneficiary structure, and classification;
 *   the siblings are separate files linked through
 *   network.affects_constraints, and nothing about them is averaged into this
 *   story. Epsilon's referent here is the standing
 *   taboo-plus-classified-planning arrangement itself, assessed by this
 *   reading's own lights — never the rights-respecting or fully-transparent
 *   alternative this reading's critics would prefer. The claimed type
 *   (tangled_rope) and the metric values were authored independently: the
 *   claim reflects my structural judgment that both a genuine coordination
 *   function and asymmetric extraction are really present; the metrics
 *   reflect my descriptive judgment of how the arrangement actually operates.
 *   Where the engine's computed per-seat types diverge from the claim, that
 *   divergence is the datum.
 *
 * KEY AGENTS:
 *   - strategic_planners: primary beneficiary (institutional/identity_locked) — write and maintain the war plans whose premises the taboo shields from public adjudication
 *   - executive_branch_leadership: agenda_setter and secondary beneficiary (institutional/arbitrage) — sets the declaratory line and controls the classification wall separating it from the plans
 *   - legislative_oversight_committees: primary payer (institutional/constrained) — formally empowered overseers who cannot quote, brief, or debate the plans' key premises
 *   - national_electorates: payer (organized/trapped) — bear the deliberative exclusion; mobilize in waves but cannot exit the arrangement
 *   - strategic_studies_gatekeepers: agenda_setter and secondary beneficiary (moderate/identity_locked) — police the boundary of the sayable in journals, seminars, and funding
 *   - arms_control_transparency_advocates: excluded (organized/mobile) — locked out of the classified conversation where the premises actually live
 *   - adversary_intelligence_establishments: observer (institutional/analytical) — read the declaratory signal and must bridge the gap to the operational layer without open-source help
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.62).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.6).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Post-1945 Winnability Taboo: Discursive Contraction with Operational Continuity").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic studies/nuclear deterrence/international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '64b3e898-b63a-4acc-aab4-488f72de12a7').
narrative_ontology:cs_kernel_codification('64b3e898-b63a-4acc-aab4-488f72de12a7', distributed).
narrative_ontology:cs_authority_grounding('64b3e898-b63a-4acc-aab4-488f72de12a7', expertise).
narrative_ontology:cs_interpretation_layer_present('64b3e898-b63a-4acc-aab4-488f72de12a7').
narrative_ontology:cs_reading_relation('64b3e898-b63a-4acc-aab4-488f72de12a7', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('64b3e898-b63a-4acc-aab4-488f72de12a7', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('64b3e898-b63a-4acc-aab4-488f72de12a7', foundational, public_articulation_of_nuclear_victory_destabilizes_deterrence).
narrative_ontology:cs_axiom_status(public_articulation_of_nuclear_victory_destabilizes_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('64b3e898-b63a-4acc-aab4-488f72de12a7', public_articulation_of_nuclear_victory_destabilizes_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('64b3e898-b63a-4acc-aab4-488f72de12a7', secondary, effective_war_planning_requires_premise_concealment).
narrative_ontology:cs_axiom_status(effective_war_planning_requires_premise_concealment, holdable).
narrative_ontology:cs_axiom_grounding('64b3e898-b63a-4acc-aab4-488f72de12a7', effective_war_planning_requires_premise_concealment, instrumental).
narrative_ontology:cs_reference_frame('64b3e898-b63a-4acc-aab4-488f72de12a7', discursive_prohibition_operational_continuity).
narrative_ontology:cs_drift_state('64b3e898-b63a-4acc-aab4-488f72de12a7', contemporary_post_2022_nuclear_discourse, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('64b3e898-b63a-4acc-aab4-488f72de12a7', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, executive_branch_leadership).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, national_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_studies_gatekeepers).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, existential_deterrence_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uniformed and civilian planners in the defense establishment write and maintain the war plans — target sets, limited-options annexes, wargame branches. Their work product is classified at levels that keep its premises out of public view, and their professional standing rests on being seen as sober custodians of terrible weapons rather than enthusiasts for using them. Speaking publicly about the conditions under which a nuclear exchange could be won would cost them that standing; retirement opens some space, but the custodian persona tends to travel with them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter).

% Presidents and defense secretaries set the declaratory line — that these weapons exist solely to prevent war — and control the classification system that separates what is said from what is planned. The gap between the two gives them room to keep options open without defending those options in public. They can reshape the declaratory line at will, which makes them the least bound seat in the arrangement.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, executive_branch_leadership, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, executive_branch_leadership, beneficiary).

% Armed services and intelligence committees hold formal authority to review war plans and the budgets that build them. In practice the plans' key premises sit in compartments members can enter but cannot quote, brief, or debate publicly, and any member who presses the usability question in open session pays a reputation cost. Oversight continues, but on terms the reviewed party largely sets.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees, payer,
    institutional, generational, constrained, national).

% Citizens underwrite the arsenal with taxes and live under its risks, but the considerations that would let them judge the planning — what the plans assume, what they target, under what theory of victory — are kept out of reach. Mobilization arrives in waves, the freeze movement being the largest, and it has moved declaratory policy; it has never reached the planning layer. There is no exit from the consequence set.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, national_electorates, payer,
    organized, biographical, trapped, national).

% The professors, think-tank analysts, and journal editors who constitute the field decide what counts as serious work. Careers inside the field were built by demonstrating one could think about horror responsibly; the boundary between sober analysis and Dr. Strangelove speculation is policed in peer review, seminar invitations, and funding decisions. Some members chafe at the boundary; few cross it twice.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_studies_gatekeepers, agenda_setter,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_studies_gatekeepers, beneficiary).

% NGO analysts, former officials turned critics, and treaty advocates argue for declassification, no-first-use declarations, and legislative audit of targeting doctrine. They operate skillfully in the public sphere, but the object of their scrutiny — the plans themselves — sits in rooms they cannot enter, and their requests for entry are routinely framed as naive or hostile to deterrence. They can redirect their energies to other issues, but the thing they want examined stays sealed.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_transparency_advocates, excluded,
    organized, generational, mobile, global).

% Opposing intelligence services watch the declaratory signal closely and try to infer the operational layer beneath it. The gap between what is said and what is planned is precisely what they must bridge, and the discursive prohibition denies them the open-source material that would help. They are spectators with stakes: their reading of the signal feeds back into their own posture decisions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, adversary_intelligence_establishments, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The taboo manages a real signaling problem: by keeping winnability out of public discourse, it reduces the chance that adversaries read open domestic debate as war preparation, spares umbrella allies the sight of patrons hedging toward limited nuclear options, and keeps the declaratory signal simple enough to bargain with during crises.
% TRANSFER_FUNCTION: Moves deliberative authority over nuclear war-fighting premises from the public sphere — legislatures, press, electorate — into closed planning channels, and moves reputational risk onto whoever breaches the taboo, so that the cost of raising the question is paid by the asker rather than the planner.
% ABSENT_VOICES: Legislative overseers without access to the relevant compartments, historians of the planning record, and the general public would object that war-fighting premises are being set without deliberation; they are outside the room because the room is classified and because the taboo makes even requesting entry look irresponsible. Adversary publics are absent for the mirror-image reason: they receive the declaratory signal without any way to test it.
% DISAPPEARANCE_RATIONALE: If the taboo and its enforcement vanished overnight, the declaratory-operational gap would close one way or the other: either planning premises would be forced into public adjudication, constraining or abandoning counterforce options the planners currently maintain unexamined, or the planners would defend them openly and the declaratory line would rewrite itself. Crisis-signaling dynamics would change immediately as adversaries gained open-source access to the thinking they currently must guess at, and the professional identities built around custodial silence would have to rebuild in the open.
% FOUNDING_PROBLEM: The arrangement was built to solve the destabilization produced by open nuclear-victory discourse in the 1950s: public debate over limited nuclear war (Kahn, Kissinger's limited-war writings) alarmed allies, handed propaganda material to adversaries, and threatened domestic political cohesion during crises, while planners simultaneously faced the genuine problem of preparing for catastrophic contingencies without normalizing them.
% FOUNDING_PROBLEM_CORROBORATION: The declassified historical record — Eisenhower-era deliberations, the 1950s open-debate literature, subsequent plan releases — corroborates from outside the benefiting parties that the original destabilization problem was real. Its present liveness is attested by crisis-simulation scholarship and adversary-behavior studies, also outside the beneficiary set; the transparency-advocacy network attests the opposite, that stabilized deterrence solved the original problem and the taboo now chiefly serves accountability avoidance. Full corroboration either way is structurally blocked by the arrangement itself, since the evidence that would settle it remains classified — a residue the taboo produces and then cites.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the arrangement transfers deliberative authority over war-fighting premises from the public sphere to closed channels, and the transfer is durable — but not total, since cleared committee staff, declassification programs, and leak-driven disclosures keep partial sightlines open. Suppression 0.60 is authored as a RAW STRUCTURAL property and is deliberately not reconciled to any scaled quantity: the engine scales only extractiveness (by directionality and scope); suppression here is normative-professional enforcement (career stigma, peer gatekeeping) layered on classification law, not mass coercion, hence below the levels seen in legally enforced snares. Theater_ratio 0.50: roughly half the arrangement's observable activity is declaratory performance — sole-purpose language, deterrence-only framing — that diverges from the operational record; the other half (actual planning, actual deterrence signaling) is functional. Accessibility_collapse 0.55: alternatives to the taboo partly survive — the 1950s open-debate tradition, FOIA litigation, foreign doctrinal publications, wargame literature — but each carries heavy stigma or security costs, so alternatives are degraded rather than eliminated. Resistance 0.50: a continuous low-grade resistance (Kahn's lineage, the freeze movement, transparency advocacy, congressional probing) that has moved declaratory policy repeatedly without ever reaching the planning layer. The temporal series runs on ONE shared nine-point grid (1950-2026) with every tracked metric authored at every point, per the alignment rule; the arc is rise-plateau-partial-decay-rehardening rather than strictly cyclical: the taboo hardened from the late 1950s through the early 1980s (extraction peaking as the declaratory-operational gap widened fastest), relaxed modestly after the Cold War's end reduced perceived stakes, and re-hardened somewhat as great-power competition returned. The 2026 endpoints are marked projected. The claim/metric pair is intentionally unreconciled: I claim tangled_rope because I judge both coordination and extraction structurally real; the engine computes per-seat types from the structural data and owns any divergence.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the planner seat the arrangement is professional responsibility: someone must prepare for catastrophes, and doing it quietly is prudence, not evasion — a beneficiary with identity_locked exit experiences the taboo as protective of seriousness itself. From the oversight seat the same structure is locked doors: formal authority without quotable premises, where asking the question in open session costs more than the answer is worth. From the gatekeeper seat it is intellectual hygiene — the boundary between sober analysis and Dr. Strangelove speculation feels like quality control from inside. From the adversary-intelligence seat it is noise: a declaratory signal whose relationship to the operational layer must be guessed. The engine derives these per-seat classifications from power, exit, and role data; the prose here explains why they diverge, not what they conclude. On coalition: the two payer seats (committees and electorates) have combined before — the freeze era — and the combination moved declaratory policy; the taboo's specific work is to fragment such coalitions by stigmatizing their premise, making oversight champions look either reckless or naive, which is why the coalition recurs episodically rather than consolidating.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners sit near the beneficiary end: they collect operational flexibility and reputational protection, and their identity_locked exit deepens the lock-in (the custodian persona makes breaching the taboo professionally self-destructive even after retirement pressure fades). Executive leadership sits nearest the beneficiary pole: agenda-setting power plus arbitrage-grade exit — they wrote the declaratory line and can rewrite it. Legislative overseers derive high directionality toward the target end: they bear the extraction (lost audit capacity) with constrained exit — they cannot abandon the oversight duty, and the taboo taxes any exercise of it. National electorates sit nearest the full-target end among the payers: trapped exit (no one exits the consequence set of nuclear planning), organized-but-episodic power. Gatekeepers carry low-to-moderate directionality as agenda_setters who also collect professional rents from administering the sayable's boundary. Transparency advocates are excluded — commentary-grade presence, not a directionality input. Adversary intelligence establishments are observers on the analytical track. Reminder carried through the scoring: suppression is unscaled structure; only extractiveness is scaled by directionality and scope, and the national-to-global scopes of the payer seats amplify their effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Read as pure snare, the taboo's genuine coordination work disappears: keeping winnability out of public discourse does manage a real collective-action problem — adversarial misreading of domestic debate as war preparation, alliance anxiety under the umbrella, crisis-period domestic political spirals — and a snare verdict would license tearing down a function someone depends on. Read as pure rope, the accountability extraction disappears: the same structure that stabilizes signals also lets planners set war-fighting premises without ever defending them, and a rope verdict would launder that. The mandatrophy interview locates the arrangement between: the founding problem (the destabilizing openness of 1950s winnability debate) is CONTESTED rather than dead — planners attest its liveness, transparency advocates attest its obsolescence, and the declassified historical record corroborates that the original problem was real while leaving the present question open. Status=contested crossed with disappearance_verdict=world_rearranges yields no dead-mandate zombie flag: the function has partially migrated (from crisis-speech management toward accountability avoidance) without atrophying, which is the tangled_rope signature, not the piton's. Theater_ratio at 0.50 is consistent with a half-performative but half-functional arrangement — well above a healthy rope, well below a piton's predominantly theatrical maintenance. Fixing cost is prohibitive (disclosure imposes genuine security externalities; abandonment forfeits capabilities planners deem necessary), which blocks the transient-neglect reading and keeps the capture-flavored cell: gains accrue to a named seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint instantiates the rhetorical_contraction reading of kernel war_winnability_post_1945; the sibling readings (deterrence_unthinkable, countervailing_thinkable) locate the disagreement in whether classified operational planning tracks physically reachable outcomes, persists as ritual, or is constrained-but-reachable behind the taboo — which sibling framing would change this story''s structural classification?',
    'Adjudicate against the declassified plan-review record and former-planner testimony: if plans show internally coherent theories of victory matching declared capabilities, the countervailing_thinkable reading gains ground; if plans show internally acknowledged incoherence or boilerplate, deterrence_unthinkable gains ground; if plans are coherent but their coherence is exactly what the taboo conceals, this reading stands.',
    'Under deterrence_unthinkable the beneficiary structure collapses (no coherent beneficiary collects; the arrangement becomes theater over an empty core, drifting piton-ward). Under countervailing_thinkable the same arrangement reads as legitimate coordination whose oversight cost is the price of realism (rope-leaning). This reading''s tangled_rope verdict holds only on the dual-layer premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: one of three readings of the winnability kernel; sibling readings would restructure beneficiary/victim topology.').

omega_variable(
    taboo_feedback_into_planning,
    'Is the dual layer fully decoupled — rhetoric contracted, strategy untouched — or does the rhetorical taboo feed back into operational planning, causing planners to under-develop or under-examine winnable options over time?',
    'Compare the option-space of successive plan generations (SIOP through current OPLANs) against contemporaneous doctrinal debates: if the classified option space narrows in step with the shrinking sayable, feedback exists; if it widens independently, the layers are decoupled.',
    'If feedback exists, the constraint is self-limiting and effective extraction declines over time (drifting toward the deterrence_unthinkable sibling); if decoupled, the dual-layer structure is stable and the accountability extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_feedback_into_planning, empirical, 'Whether the discursive contraction propagates into the operational layer.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression of winnability discourse structural (classification rules, career gatekeeping, funding leverage) or internalized (strategists who genuinely believe discussing victory is irresponsible, independent of any sanction)?',
    'Post-exit suppression trajectory: track retired planners and emeritus analysts who face no remaining sanction — if they breach the taboo freely in memoirs and seminars, suppression was structural; if they continue to self-censor in the same vocabulary, it was internalized.',
    'If largely internalized, classification reform alone will not reopen the discursive space — effective suppression exceeds the structural measure and persists after the enforcement machinery is dismantled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the taboo''s suppressive force.').

omega_variable(
    crisis_instability_tradeoff,
    'Would reopening winnability discourse raise crisis instability (adversarial misreading of open debate as war preparation) enough to outweigh the accountability gains — i.e., is the taboo''s coordination function worth its extraction?',
    'Crisis-simulation experiments varying discourse openness, plus historical analogues (the semi-open 1950s debate, Berlin and Cuba episodes) where winnability talk circulated while crises ran.',
    'If instability costs dominate, the coordination function is real and load-bearing and the tangled_rope verdict firms up; if they are negligible, the taboo''s coordination story is cover and the constraint slides toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_instability_tradeoff, empirical, 'The coordination-versus-extraction boundary question for the taboo.').

omega_variable(
    cross_nuclear_power_variation,
    'Does the taboo bind uniformly across nuclear-armed states, or does it vary (Russian and Chinese doctrinal publications discuss nuclear employment far more openly than US/UK practice), changing the constraint''s scope profile?',
    'Comparative coding of published doctrine, parliamentary oversight records, and civil-military discourse norms across the recognized nuclear powers.',
    'If the taboo is Anglo-American-specific, scope-based extraction amplification applies unevenly and the story''s global scope attribution overstates the constraint for some seats; if it generalizes, the global scope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_nuclear_power_variation, empirical, 'Cross-national variation in the sayability of nuclear winnability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wwp45_rhet_contract_tr_t1950, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t1950, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.24).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t1960, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t1970, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1970, 0.36).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t1970, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.52).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t1980, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t1990, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1990, 0.56).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t1990, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.5).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t2000, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t2010, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2010, 0.47).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t2010, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t2020, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2020, 0.51).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t2020, observed).
narrative_ontology:measurement(wwp45_rhet_contract_tr_t2026, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2026, 0.5).
narrative_ontology:measurement_basis(wwp45_rhet_contract_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(wwp45_rhet_contract_be_t1950, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t1950, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.34).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t1960, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t1970, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t1970, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t1980, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t1990, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1990, 0.64).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t1990, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t2000, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t2010, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t2010, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t2020, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t2020, observed).
narrative_ontology:measurement(wwp45_rhet_contract_be_t2026, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(wwp45_rhet_contract_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(wwp45_rhet_contract_su_t1950, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t1950, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t1960, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t1970, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1970, 0.54).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t1970, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t1980, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t1990, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t1990, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t2000, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t2010, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t2010, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t2020, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t2020, observed).
narrative_ontology:measurement(wwp45_rhet_contract_su_t2026, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(wwp45_rhet_contract_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'nuclear winnability after 1945'. The single natural-language concept conflates three structurally distinct claims with materially different epsilons: deterrence_unthinkable (planning is incoherent; negligible extraction, high theater), countervailing_thinkable (limited victory is reachable; low extraction, genuine coordination), and this story, rhetorical_contraction (the sayable contracted while the plannable persisted; moderate-high extraction from democratic oversight riding on a real signaling-coordination function). Upstream/downstream structure: the countervailing_thinkable claim is what the concealed planning presupposes, and this reading's taboo is what suppresses its public articulation — the taboo changes the legitimacy conditions under which countervailing claims may be voiced (channeling them into euphemism such as 'deterrence credibility'), which is modeled as an influences edge in cs_structure.reading_relations. Each family member links the others via network.affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
