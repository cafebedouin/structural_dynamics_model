% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Total War Taboo (Normative Prohibition Reading)
 *   domain: international/strategic/institutional
 *
 * SUMMARY:
 *   This constraint instantiates a normative reading of the nuclear total-war
 *   prohibition. After 1945, when nuclear weapons made total war materially
 *   possible and (under traditional war logic) strategically rational, an
 *   international norm emerged that foreclosed total war as a permissible
 *   strategic option. This reading asserts that the taboo is the operative
 *   mechanism of prevention—war remains materially possible and strategically
 *   rational in the technical sense, but is prevented by constructed and
 *   enforced normative structure. The global norms-custodian institutions
 *   (the UN system, the non-proliferation treaty regime, arms-control bodies,
 *   epistemic communities) actively curate the taboo through treaty
 *   enforcement, rhetorical affirmation, and isolation of norm violators.
 *   Nuclear-armed powers internalize the constraint and maintain continuous
 *   alignment with the norm framework. Non-nuclear states benefit from the
 *   taboo by gaining survival assurance; their exit is foreclosed by material
 *   reality (they cannot build nuclear forces of equivalent power). The
 *   constraint is extractive for nuclear powers (who relinquish escalatory
 *   options) and for some regional actors (who are identity-locked into taboo
 *   compliance). The claimed type is tangled_rope because the constraint
 *   simultaneously coordinates a genuine collective interest (preventing
 *   existential warfare) and extracts strategic optionality from those it
 *   constrains. The metrics show extractiveness declining over the interval
 *   (initial conflict between material capability and norm nascence resolved
 *   toward norm internalization) and theater increasing (the maintenance
 *   activity—non-first-use pledges, diplomatic affirmations,
 *   crisis-management protocols—has become more performative relative to
 *   functional enforcement as the taboo has hardened). This is ONE reading of
 *   a contested kernel; the sibling readings (deterrence_equilibrium and
 *   space_contraction) explain the same phenomenon (no total war occurs)
 *   through different causal mechanisms.
 *
 * KEY AGENTS:
 *   - Major nuclear powers (US, Russia, China, France, UK): hold strategic optionality but are constrained by the taboo; bear costs of continuous norm alignment; benefit from mutual taboo stability.
 *   - Non-nuclear states (vast majority of UN members): depend entirely on the taboo for survival; have no voice in governing it; benefit from it without maintenance cost.
 *   - Global norms-custodian institutions (UN Security Council, NPT regime, IAEA, arms-control expertise networks, academic disciplines of strategic studies and international relations): set and enforce the taboo through institutional architecture; administer the constraint.
 *   - Nuclear-armed regional actors (India, Pakistan, Israel, North Korea): occupy an ambiguous structural position—they have nuclear weapons but face different taboo pressures than great powers; identity-locked into norm compliance but with weaker enforcement than major powers face.
 *   - Excluded parties (states in existential conflicts where nuclear escalation would be rational; disarmament advocates; norm-breakdown scenarios): would argue against the taboo or for removing it; have no seat at the constraint's governance table.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.31).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.68).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Total War Taboo (Normative Prohibition Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international/strategic/institutional").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '9b30f75b-fae6-4451-aee8-8b76e06b9fa2').
narrative_ontology:cs_kernel_codification('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', distributed).
narrative_ontology:cs_authority_grounding('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', extraction).
narrative_ontology:cs_interpretation_layer_present('9b30f75b-fae6-4451-aee8-8b76e06b9fa2').
narrative_ontology:cs_reading_relation('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', foundational, total_war_prevented_by_normative_prohibition).
narrative_ontology:cs_axiom_status(total_war_prevented_by_normative_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', total_war_prevented_by_normative_prohibition, conventional).
narrative_ontology:cs_axiom('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', secondary, taboo_depends_on_institutional_curation).
narrative_ontology:cs_axiom_status(taboo_depends_on_institutional_curation, holdable).
narrative_ontology:cs_axiom_grounding('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', taboo_depends_on_institutional_curation, empirically_contingent).
narrative_ontology:cs_reference_frame('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', mutual_assured_destruction_taboo).
narrative_ontology:cs_drift_state('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', contemporary_strategic_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b30f75b-fae6-4451-aee8-8b76e06b9fa2', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_norms_custodians).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, major_nuclear_powers).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, nuclear_armed_regional_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Strategic nuclear forces remain materially available and capable of total war, but are operationally and rhetorically locked out by an internalized norm structure. They bear the costs of maintaining this taboo through continuous rhetorical alignment, non-first-use pledges, non-proliferation treaty adherence, and crisis management protocols that prevent escalation. The taboo constrains their coercive options even when nuclear use would be materially rational under war-logic analysis. They pay through restricted strategic choice, though they also benefit from mutual taboo stability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, major_nuclear_powers, payer,
    institutional, civilizational, constrained, global).

% Depend on the taboo for survival because they cannot credibly deter total war through material capability. The norm structure ensures that the largest power asymmetries remain bounded by taboo rather than resolved through annihilation. They benefit from the norm's persistence without maintaining it; their exit is foreclosed by the material world (no nuclear force of their own can substitute).
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    powerless, civilizational, trapped, global).

% International institutions, epistemic communities, diplomatic networks, and academic traditions (treaty bodies like the NPT regime, the UN Security Council, arms control expertise networks, historical memory institutions) actively curate and enforce the taboo through treaty architectures, rhetorical affirmation in state communications, normalization of "no-first-use" as the default posture, and isolation of any actor who breaches norm language. They set and administer the constraint by deciding what counts as taboo violation, what justifications are permitted, and what penalties apply to violation signals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_norms_custodians, agenda_setter,
    institutional, civilizational, analytical, global).

% Possess nuclear weapons, conferring material capability for devastating escalation, but are locked into taboo compliance by regional acceptance requirements and international legitimacy claims that are identity-constituting. Exiting the taboo would mean repudiation by the international order and by their own domestic populations who have internalized the norm. The taboo constrains their regional war options; a conventional regional war remains available, but nuclear escalation as a coercive tool is foreclosed. Their identity as a responsible nuclear power—essential to their standing—depends on taboo maintenance.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_armed_regional_actors, payer,
    powerful, generational, identity_locked, global).

% Would argue that the taboo is insufficient (nuclear weapons should be eliminated entirely), that the taboo creates moral hazard (permitting conventional war under the assumption nuclear bounds hold), and that taboo persistence requires active norm defense work that is politically precarious. They are excluded from the constraint's governance because the major powers and norms-custodian institutions do not admit disarmament as a negotiable option; the taboo is treated as permanent, not provisional.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, disarmament_advocates, excluded,
    organized, civilizational, analytical, global).

% States that might benefit strategically from nuclear escalation (those in existential conflicts where nuclear use would be rational under pure strategic calculation) but are excluded from exercising that option by the taboo and by the enforcement mechanisms that would penalize breach. Their exclusion is active: the international order would treat nuclear use as a civilization-level crime, not a legitimate strategic move. They have no voice in taboo governance; the norm structure operates against their interests.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, adversarial_states_coalition, excluded,
    organized, generational, constrained, global).

% Counterfactual positions: scenarios where the taboo erodes (norm entrepreneurs exit, key custodian institutions fail, a breach goes unpunished and normalizes escalation). Included for completeness to track the structural fragility of the arrangement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_collapse_scenarios, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__nuclear_taboo_reading, norm_collapse_scenarios).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, global_norms_custodians).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing nuclear escalation in interstate conflict: all major powers benefit from a world where large power differentials do not resolve into existential warfare, and the taboo coordinates that mutual preference into stable practice without requiring continuous explicit negotiation or threat balance.
% TRANSFER_FUNCTION: Moves strategic coercive options from major nuclear powers to non-nuclear states and regional actors. Nuclear powers relinquish the ability to escalate beyond conventional bounds; non-nuclear states gain survival assurance because the strongest powers have voluntarily bound themselves. Regional nuclear actors lose escalatory freedom but gain legitimacy from alignment with the global norm.
% ABSENT_VOICES: Actors who would benefit materially from nuclear escalation (states in existential regional conflicts, would-be hegemons with revisionist aims, proliferating nations treating nuclear weapons as deterrent-of-last-resort) are structurally excluded. They have no seat in the NPT regime or major arms-control deliberations. If admitted, they would argue for removing the taboo or carving exceptions for existential defense—arguments that would destabilize the arrangement.
% DISAPPEARANCE_RATIONALE: If the nuclear total-war taboo disappeared overnight (norms custodians stopped enforcing it, major powers renormalized escalation rhetoric, no-first-use pledges were abandoned), interstate conflict calculus would reorganize around material capability. States with nuclear forces would face existential security trade-offs currently foreclosed. Non-nuclear states would lose survival assurance and would pursue nuclear acquisition or shift to non-state security arrangements. The international order would fragment rapidly as mutual vulnerability replaced mutual taboo.
% FOUNDING_PROBLEM: After 1945, the scale of destructive capability made total war—the annihilation of an adversary's state and population—materially achievable and strategically rational under traditional war logic, but produced a mutually suicidal equilibrium. The founding problem was: how do we prevent rational actors from choosing annihilation when capability makes it possible?
% FOUNDING_PROBLEM_CORROBORATION: The major nuclear powers attest the problem persists and cite their continuous maintenance of non-first-use norms and non-proliferation commitments as evidence that the taboo is necessary and functional. Strategic studies scholars outside the military establishments attest that the founding problem is partially solved by the taboo but remains precarious—that the taboo itself is the solution, not material capability alone. Disarmament advocates argue the problem is not solved but merely managed and could re-emerge if norm maintenance fails. No authoritative external voice (a neutral referee) exists; the problem's status is read differently depending on the reader's structural position.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31) because the taboo does coordinate a genuine collective interest (preventing total war) and most parties accept the norm as legitimate, but it simultaneously relinquishes strategic optionality from nuclear powers, and it extracts survival advantage unequally across states (non-nuclear states depend entirely on it; nuclear powers retain deterrent value). Suppression is high (0.68) because the constraint's persistence depends on active enforcement: non-proliferation regime enforcement against violators, diplomatic isolation of norm-breach rhetoric, continuous reaffirmation of no-first-use pledges, crisis-management protocols that enforce escalation-stop points. Accessibility_collapse is high (0.72) because once the taboo is understood, alternatives to nuclear-bounded war are extremely narrow—states can wage conventional war (still open) but cannot escalate to existential nuclear exchange without massive international penalties and self-imposed strategic constraints. Resistance is moderate (0.58) because the taboo has strong institutional backing and norm internalization in major powers, but it faces persistent pressure from regional actors, revisionist powers, and deterrence-theorists who argue the taboo is fragile or illegitimate. Theater_ratio is moderate (0.42) because a substantial share of the enforcement activity is performative—no-first-use pledges repeated at every diplomatic occasion, arms-control treaties signed and re-signed, international statements condemning nuclear proliferation—but the underlying mechanism (mutual taboo internalization) remains functional. The measurements show extractiveness declining from 1945 (when the constraint was newly imposed and resistance was high) to 1991 (when the Cold War ended and the taboo was deeply internalized by both superpowers), then stabilizing. Theater increases over the same period because the active enforcement need decreases as internalization hardens; what remains is mostly maintenance activity and rhetorical performance. Suppression_requirement rises from 1945 to 1991 (as the regime's enforcement machinery builds) then stabilizes. This temporal profile is consistent with a constraint moving from contested external imposition toward internalized norm structure.
 *
 * PERSPECTIVAL GAP:
 *   From the major nuclear powers' seat: the taboo is a beneficial coordination mechanism that prevents rational-but-catastrophic escalation; they experience it as a legitimate constraint that they help maintain. From the non-nuclear states' seat: the taboo is a survival mechanism imposed by those with nuclear weapons; they experience it as a dependency on great-power forbearance, not as a symmetric coordination. From the regional nuclear actors' seat: the taboo is an ambiguous constraint—it confers legitimacy (alignment with global norms) but also locks them into a strategic position where nuclear use is rhetorically foreclosed even in existential situations. From the disarmament advocates' seat: the taboo is insufficient and masks an extractive arrangement that preserves major-power strategic advantage. The engine computes these perspectival differences from the structural data (power level, exit options, beneficiary/victim status); the authoring makes no claim about which perspective is correct, only that they differ and are derivable from the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Major nuclear powers: beneficiaries in the sense that the taboo prevents adversaries from escalating to existential warfare, and it preserves great-power strategic dominance. But they are also payers because the taboo constrains their own escalatory options. The net directionality is near symmetric for institutional powers with arbitrage (they can adapt deterrent strategy without nuclear use). Non-nuclear states: full beneficiaries (d near 0.0) because they depend entirely on the taboo without bearing maintenance costs. Their exit is trapped—they cannot acquire nuclear forces to deter existentially. Global norms custodians: agenda-setters who set and enforce the constraint; they benefit from administering it (institutional purpose, budgets, influence). Regional nuclear actors: complex position. They are payers in that the taboo constrains their coercive optionality; they are beneficiaries in that alignment with the norm confers international legitimacy. But they are also identity-locked (exit = international pariah status and loss of domestic legitimacy). Directionality for regional actors is near the target end (d around 0.6–0.7) because their strategic choice is constrained and their only real option is conformity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do we prevent rational actors from choosing annihilation when capability makes it possible?) was acute in 1945–1962 and genuinely live. The measured founding_problem_status is contested because major powers, academic strategists outside benefiting institutions, and disarmament advocates read the problem's status differently. This is structurally sound for a tangled_rope: the coordination function (preventing mutual suicide) is real and beneficial, so the founding problem remains live as a justification. The classification does not collapse to snare because the taboo solves a genuine problem (major powers would not be better off if total war became normalized), even though it extracts some strategic optionality and asymmetrically benefits non-nuclear states. The mandatrophy question (is the constraint maintained because it still solves the founding problem, or because the maintaining institution has acquired independent interest in maintaining it?) is empirically unresolved. The rising theater_ratio (0.08 to 0.42) suggests the enforcement function is increasingly performative—the taboo persists because it is institutionally embedded and norm-internalized, not because anyone is actively rebuilding the prevention mechanism. But this is evidence of institutional stability and norm internalization, not institutional capture or mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_deterrence_equilibrium,
    'How does the nuclear_taboo_reading relate structurally to the deterrence_equilibrium_reading, which explains total-war prevention through mutual vulnerability rather than normative taboo?',
    'The readings differ in their causal mechanisms (taboo vs. rational deterrence) and in their stability predictions (taboo could fail if norms decay; deterrence persists as long as both sides remain rational). Test via state rhetoric and decision-theory analysis: taboo readers treat nuclear use as ''forbidden but possible''; deterrence readers treat it as ''irrational but possible''; space_contraction readers treat it as ''cognitively unavailable''. Crisis decision-making shows evidence of all three framing simultaneously in different institutional contexts.',
    'If the deterrence_equilibrium reading is the correct causal mechanism, then the taboo reading has misidentified the operative constraint. The classification could shift from tangled_rope (taboo + coordination) to rope (deterrence-based mutual preference). The practical difference: deterrence-based prevention is robust to norm decay but fragile to breakdown of rationality or to shifts in cost-benefit calculations (proliferation, changed power distributions). Taboo-based prevention is robust to rationality but fragile to norm decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_deterrence_equilibrium, conceptual, 'Relationship between this reading and the deterrence_equilibrium sibling: distinct causal mechanisms, coexisting frameworks.').

omega_variable(
    sibling_reading_space_contraction,
    'How does the nuclear_taboo_reading relate structurally to the space_contraction_reading, which asserts that nuclear weapons removed total war from the strategically thinkable possibility space?',
    'The readings differ in their structural claims: taboo_reading asserts total war is possible but prohibited; space_contraction_reading asserts total war is impossible (not thinkable, not choosable). This is an ontological difference about the nature of the constraint itself. Empirical test: do strategic planners and military decision-makers treat nuclear escalation as a ''live branching point that we choose not to take'' (taboo reading) or as ''off the possibility menu entirely'' (space_contraction reading)? Evidence from classified strategic planning documents and from crisis-period decision-maker interviews could resolve this, but is not publicly accessible.',
    'If space_contraction is the correct reading, then total war is a mountain-type constraint (materially/strategically impossible), not a constructed taboo. The classification would shift from tangled_rope to mountain. Practical difference: a mountain-type constraint persists without active maintenance; a taboo-type constraint requires continuous institutional enforcement and could fail if custodians exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_space_contraction, conceptual, 'Relationship between this reading and the space_contraction sibling: ontological difference about what total war is (possible-but-prohibited vs. strategically-impossible).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement_basis(tota_tr_t1945, projected).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.18).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1979, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1979, 0.28).
narrative_ontology:measurement_basis(tota_tr_t1979, observed).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1991, 0.38).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2006, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2006, 0.41).
narrative_ontology:measurement_basis(tota_tr_t2006, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement_basis(tota_be_t1945, projected).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.42).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1979, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement_basis(tota_be_t1979, observed).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1991, 0.28).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2006, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2006, 0.31).
narrative_ontology:measurement_basis(tota_be_t2006, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(tota_su_t1945, projected).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.62).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1979, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1979, 0.68).
narrative_ontology:measurement_basis(tota_su_t1979, observed).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1991, 0.71).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2006, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2006, 0.69).
narrative_ontology:measurement_basis(tota_su_t2006, observed).
narrative_ontology:measurement(tota_su_t2026, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(tota_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_structural_effect).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, regional_nuclear_actor_constraint_structure).

% DUAL FORMULATION NOTE:
% The 'total_war_possibility_space' kernel admits three structurally distinct readings: nuclear_taboo_reading (this story), deterrence_equilibrium_reading, and space_contraction_reading. Each reading instantiates a different constraint with different causal mechanisms and different stability properties. They are not sequential refinements of a single claim; they are competing analytical frameworks held by different epistemic communities (taboo reading: international-relations institutionalists; deterrence reading: strategic theorists; space_contraction reading: some strategic studies and philosophy-of-physics scholars). Each sister story carries its own ε, its own beneficiary/victim structure, and its own type classification. Link via network.affects_constraints to enable contamination analysis: a shift toward deterrence-equilibrium framing would loosen the enforcement mechanisms this story predicts; a shift toward space_contraction would imply the constraint is more robust than taboo maintenance suggests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
