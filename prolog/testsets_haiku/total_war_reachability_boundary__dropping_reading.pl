% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Reading)
 *   domain: international_relations/strategic_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates the 'dropping_reading' of the contested
 *   kernel 'total_war_reachability_boundary.' The dropping reading
 *   characterizes total war as physically and strategically reachable (the
 *   capability persists, accident pathways exist) but with dramatically
 *   lowered probability due to mutual vulnerability equilibrium. Deterrence
 *   is framed as a rope-type coordination: genuine problem solved
 *   (great-power conventional total war made improbable), genuine
 *   beneficiaries (states that maintain deterrence credibility, populations
 *   that avoid major interstate wars), but also genuine extraction (civilian
 *   hostage status, existential risk borne by populations who did not choose
 *   the system). The claim/metric divergence is intentional: the constraint
 *   is claimed as tangled_rope (coordination plus asymmetric extraction)
 *   while the authored metrics describe a moderately extractive, actively
 *   enforced system with growing theatrical component (increasing
 *   theater_ratio from 1945 to present) — the engine measures whether this
 *   characterization is empirically defensible.
 *
 * KEY AGENTS:
 *   - Deterrence Creditor States (nuclear-armed powers): set doctrine, maintain arsenals, benefit from coordination equilibrium that keeps sub-total-war competition within bounds.
 *   - Civilian Populations: bear existential hostage status, extract zero coordination benefit, carry non-exit constraint.
 *   - Military Establishments: professional identity locked to deterrence doctrine; gate their own exit.
 *   - Non-Nuclear States: excluded from core coordination; constrained military options; forced alignment with nuclear patron or acceptance of vulnerability.
 *   - Arms Control Regimes: structurally excluded from deterrence decisions; advocating alternatives that undermine deterrence credibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '6c6c8e96-cb98-4f1f-8292-c8fa16b6344e').
narrative_ontology:cs_kernel_codification('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', distributed).
narrative_ontology:cs_authority_grounding('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', extraction).
narrative_ontology:cs_interpretation_layer_present('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e').
narrative_ontology:cs_reading_relation('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', foundational, reachability_persistent_but_improbable).
narrative_ontology:cs_axiom_status(reachability_persistent_but_improbable, holdable).
narrative_ontology:cs_axiom_grounding('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', reachability_persistent_but_improbable, empirically_contingent).
narrative_ontology:cs_axiom('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', foundational, deterrence_equilibrium_genuine_coordination).
narrative_ontology:cs_axiom_status(deterrence_equilibrium_genuine_coordination, holdable).
narrative_ontology:cs_axiom_grounding('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', deterrence_equilibrium_genuine_coordination, instrumental).
narrative_ontology:cs_reference_frame('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', mutual_vulnerability_equilibrium_stable).
narrative_ontology:cs_drift_state('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6c6c8e96-cb98-4f1f-8292-c8fa16b6344e', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, deterrence_creditor_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_stability_beneficiaries).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, military_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear-armed states that maintain strategic deterrence doctrine and arsenals. They set the terms of the deterrence game by maintaining credible second-strike capability and communicating resolve. They benefit from the coordination equilibrium that keeps total war reachable but unlikely — the threat of mutual destruction creates stability that allows their non-military interests to pursue advantage at sub-total-war intensities. They cannot exit: abandoning nuclear capability shifts the strategic balance unpredictably.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, deterrence_creditor_states, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, deterrence_creditor_states, beneficiary).

% Inhabitants of major population centers in nuclear-armed or allied states. They bear the existential extraction of the deterrence system: they are the implicit hostages whose vulnerability underwrites credible deterrence. Their exit options are zero — they cannot leave the jurisdiction or move out of range of nuclear weapons. The coordination benefit (strategic stability preventing conventional total war) flows to states and elites; the extraction (living under nuclear threat) is borne diffusely by populations.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat, payer,
    powerless, biographical, trapped, global).

% International institutions, non-aligned states, and civilian populations that benefit from the reduction in great-power conventional warfare probability created by nuclear deterrence. They did not choose the system but gain from its coordination function: major interstate wars have become rare. They have modest exit options (changing international alignments, hedging strategies, arms development) but participate in the deterrence system without directly controlling it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_stability_beneficiaries, beneficiary,
    organized, generational, mobile, global).

% Countries without nuclear weapons that operate within the deterrence framework established by nuclear-armed states. They face extraction in two forms: (1) they are excluded from the coordination function and must accept nuclear-armed states as security guarantors or threats, and (2) their military options are constrained by the nuclear threshold — they cannot escalate to total war even if locally overwhelmed. Their exit options are limited: develop nuclear capability (difficult, costly, opposed by existing nuclear powers), align with a nuclear patron (accepts subordination), or develop asymmetric strategies (terrorism, insurgency, cyber — all inferior to conventional total war by military calculus).
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    powerful, generational, constrained, global).

% Officer corps and defense establishments in nuclear-armed and aligned states. They have professional identity fused with deterrence doctrine — their careers, strategic doctrine, and institutional prestige ride on the credibility of nuclear deterrence. They benefit from the system's persistence: it justifies defense budgets, strategic planning, and institutional hierarchy. Exit is identity-locked: renouncing deterrence doctrine would dissolve the professional framework they inhabit.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, military_establishments, beneficiary,
    organized, biographical, identity_locked, global).

% International treaties and institutions (NPT, START, CTBT frameworks) that would regulate or reduce the reachability of total war. They are structurally excluded from the core deterrence decision — states retain sovereignty over strategic force posture. Their objections to reachability are subordinate to state security interests. They could reshape the constraint if states chose to bind themselves, but that choice depends on the credibility of alternatives to deterrence, which the deterrence system itself undermines.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_regimes, excluded,
    institutional, generational, analytical, global).

% External analyst tracking the strategic structure. Observes that deterrence functions as a coordination game where mutual vulnerability creates an equilibrium favoring sub-total-war competition, but that equilibrium is unstable under perturbations (accident, miscalculation, regime change, emerging technologies). The reachability of total war is the residual risk the system generates.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, deterrence_creditor_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a mutual vulnerability equilibrium that reduces the probability of great-power total war by making the cost of escalation to nuclear exchange unacceptable to all parties. Enables sub-total-war competition (proxy conflicts, economic rivalry, alliance posturing) within a strategic frame where direct conflict between nuclear powers carries mutual destruction risk. Solves the problem of how major powers can contest interests without triggering wars of annihilation.
% TRANSFER_FUNCTION: Moves existential risk from state elites (who make strategic decisions) to civilian populations (who inhabit the targeting zones). Nuclear deterrence shifts the basis of state power from military conquest to maintained capability to destroy adversaries' populations. Elites and strategic establishments gain stability and legitimacy; populations bear the continuous threat and latent extraction of hostage status.
% ABSENT_VOICES: Populations under the nuclear umbrella are not represented in strategic doctrine formation or force-posture decisions. Non-nuclear states that depend on the nuclear umbrella for security guarantees are subordinated to decisions made by the nuclear-armed power. Future generations who will inherit or be destroyed by the system are not present in current strategic planning. Arms control advocates and peace movements object but are structurally outside deterrence decision-making (their exclusion is the excluded role in the stakeholder model).
% DISAPPEARANCE_RATIONALE: If the deterrence system and the reachability of total war vanished overnight — if states could credibly believe that total war was no longer possible and mutual vulnerability was broken — strategic incentives would restructure radically. States would recalculate conventional military strategies, alliance postures would shift as security guarantees lost their nuclear backing, and major-power conflict probability would increase substantially (though not necessarily to pre-nuclear levels, as conventional destructiveness alone might still deter). The world would rearrange itself around new assumptions about what conflicts are risked.
% FOUNDING_PROBLEM: After nuclear weapons emerged, states faced a coordination problem: how to manage great-power rivalry when direct conflict could trigger mutual annihilation. Total war (war of national survival, unlimited destruction) became strategically unreachable in the sense that no rational actor would initiate it; but it remained physically reachable — the capability to fight it, and the pathway to it through miscalculation or accident, persisted. The founding problem was to create a stable equilibrium where this reachability-but-improbability condition was maintained without requiring conscious restraint by all parties at every moment.
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists (Schelling, Waltz, Jervis) from outside any nuclear-armed state's defense establishment attest the coordination problem is ongoing. Doctrinal statements from all five permanent UN Security Council members acknowledge the deterrence rationale. Non-aligned movement states and arms-control advocates attest the problem is being managed but remains unsolved — reachability persists and could increase with technological change or regime instability. No party credibly attests that the founding problem has been solved; they dispute whether deterrence is the right solution.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68, rising from 0.82 in 1945, plateauing from 2000 onward) reflects the trajectory of deterrence credibility. Early in the nuclear era, extraction was higher because deterrence was novel and brittle — populations bore maximum existential risk under minimal institutional reassurance. As arms control regimes, crisis management protocols, and strategic doctrine matured, the probability of accidental war dropped, and extraction moderated. The plateauing from 2000–2026 suggests the system has stabilized at a lower-extraction equilibrium, though reachability remains. Suppression (0.71, rising from 0.55 in 1945) reflects increasing enforcement overhead: maintaining deterrence credibility requires continuous surveillance, doctrine refinement, force modernization, and political messaging. Theater_ratio (0.42, rising from 0.25 in 1945, plateauing from 2000 onward) indicates growing performative component — strategic messaging, exercises, doctrinal posturing consuming increasing share of enforcement activity relative to core deterrence function. The measurements are authored on a single shared time grid: every metric is assessed at 1945, 1962, 1979, 2000, 2015, and 2026, enabling temporal analysis of coupled drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (deterrence creditor states) seat should compute as strongly beneficiary-biased (low directionality toward target end) because the constraint's benefits accrue directly to the state apparatus maintaining it. The payer seats (civilian populations, non-nuclear states) should compute as strongly target-biased (high directionality toward target end) because extraction flows away from them without meaningful exit. The excluded seat (arms control regimes) should compute as low-power payer despite institutional framing. These divergences are structural: the same constraint looks like valuable coordination from the deterrence-maintaining seat and like imposed hostage status from the civilian-population seat. The engine derives directionality from beneficiary/victim declarations and exit_options; the authored structural data (agenda_setter role for deterrence creditor states, payer role for civilian populations, trapped exit for both) should produce per-seat type divergence automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   Deterrence creditor states: agenda_setter + institutional + trapped exit + beneficiary role → directionality strongly toward 0.0 (full beneficiary). They set the rules, extract benefits (strategic stability, arms sales, geopolitical leverage), and cannot exit without cascading security consequences. Civilian populations: powerless + payer role + trapped exit → directionality strongly toward 1.0 (full target). They bear extraction (existential threat, involuntary hostage status) with zero exit options. Non-nuclear states: powerful/organized + payer role + constrained exit → directionality moderately toward 1.0 (partial target). They pay through subordination to nuclear patron or vulnerability to nuclear-armed rivals; exit options exist (nuclear development, alignment change) but are costly and opposed. Military establishments: organized + beneficiary role + identity_locked exit → directionality near 0.5 (symmetric, biased toward beneficiary). They benefit from deterrence credibility but their professional identity is fused to the system, so they also bear extraction (if deterrence failed, their doctrine would be discredited). The identity lock prevents clean exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as a pure snare (extraction without genuine coordination) by virtue of the authentic coordination function: deterrence reduces the probability of great-power conventional total war, which is a genuine coordination problem solved. However, it avoids pure classification as rope (coordination with minimal coercive overhead) because substantial active enforcement (military posture, doctrine refinement, surveillance, strategic messaging) is required to maintain credibility, and extraction flows asymmetrically to populations who did not consent and cannot exit. The tangled_rope classification captures this: genuine coordination function (rope) plus asymmetric extraction (snare-like), held together by active enforcement. Mandatrophy would be declared if the founding problem (how to manage great-power rivalry when mutual destruction is possible) became obsolete — e.g., if technological change made total war truly unreachable even to irrational actors, or if states achieved genuine disarmament. The measurement trajectory suggests no mandatrophy has occurred: the founding problem remains live (as attested in six_questions.founding_problem_status), and the constraint persists at lower extraction than initially but with stabilized enforcement overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_rationality,
    'Is total war truly remaining ''reachable'' (physically and logically possible) if all rational actors will refuse to initiate it, or does rational universality of non-initiation constitute a form of structural unreachability?',
    'Philosophical/logical analysis of reachability definitions in game theory and strategic doctrine. Empirical observation of whether states invest in capabilities they will never rationally use (a positive finding would support physical reachability despite rational unreachability).',
    'If rational actors define reachability (rationality unreachable = effectively unreachable), this reading shifts toward mountain-type stability; if physical capability defines reachability, the constraint remains extractive because populations remain hostages to low-probability, high-consequence events. This determines whether the coordination is genuine or illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_vs_rationality, conceptual, 'Whether reachability is defined by capability or by rational behavior under common knowledge.').

omega_variable(
    deterrence_stability_fragility,
    'How fragile is the deterrence equilibrium under perturbations: regime change, technological disruption (hypersonic delivery, AI decision-making, space-based systems), or deliberate revisionism by an emerging nuclear power?',
    'Scenario modeling from defense analysts and strategists; historical case studies of crisis stability breakdowns (Cuban Missile Crisis near-miss data, Cold War incidents); empirical observation of how new nuclear powers behave relative to equilibrium predictions.',
    'A demonstrably fragile equilibrium (high sensitivity to perturbations) would increase the effective extraction experienced by civilian populations — they bear rising existential risk even as stated probability of war drops. A stable equilibrium supports the constraint''s coordination framing. This feeds the suppression_requirement trajectory: if stability requires increasing enforcement overhead (more surveillance, more arms control verification, more strategic messaging), the constraint transitions toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_fragility, empirical, 'Empirical fragility of the mutual-vulnerability equilibrium under realistic perturbations.').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates ONE reading of the ''total_war_reachability_boundary'' kernel. Does total war''s reachability reflect a permanent structural feature (contraction_reading: nuclear weapons removed it from feasible set), an atrophied capability that could reverse (contingent_reachability_reading: piton maintained by current tech/doctrine), or a dropping but persistent coordinate of strategic space (this reading: tangled_rope coordination)?',
    'Long-term observation of whether reachability remains constant, decreases monotonically, or exhibits reversals as technology and doctrine evolve. Cross-reference with the sibling readings'' predictions: if contingent_reachability emerges true (reversals observed), this reading mischaracterized the kernel; if contraction is true (reachability provably impossible), this reading overstated persistence.',
    'If this reading is correct and the kernel is validly described as a dropping but reachable coordination, then deterrence theory''s framing as ''rope plus mutation risk'' is accurate. If sibling readings prove superior, the constraint''s classification shifts and the structural analysis of beneficiary/victim/extraction changes. This omega carries the epistemic weight of the kernel contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the total_war_reachability_boundary kernel''s structure matches the dropping_reading characterization or favors a sibling reading.').

omega_variable(
    suppression_internalization,
    'Is the suppression measured (0.71) primarily structural (enforcement of deterrence doctrine, military posture, nuclear force readiness) or internalized (civilian populations have incorporated the threat into their psychological baseline and no longer actively resist it)?',
    'Post-deterrence transition scenario: if deterrence doctrine were abandoned, would suppression persist in populations? Measurement of active resistance to nuclear force posture (protest, defection from military, political pressure) versus passive acceptance.',
    'If suppression is primarily internalized, civilian populations carry the constraint with them even if the institutional structure weakened — they have become habituated to existential threat. If primarily structural, removing enforcement would reduce suppression. The distinction determines whether the constraint''s hold depends on active maintenance or on institutional-cum-psychological embedding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural (external enforcement) or internalized (psychological habituation to threat).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_drop_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(twrb_drop_tr_t1945, observed).
narrative_ontology:measurement(twrb_drop_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.35).
narrative_ontology:measurement_basis(twrb_drop_tr_t1962, observed).
narrative_ontology:measurement(twrb_drop_tr_t1979, total_war_reachability_boundary__dropping_reading, theater_ratio, 1979, 0.4).
narrative_ontology:measurement_basis(twrb_drop_tr_t1979, observed).
narrative_ontology:measurement(twrb_drop_tr_t2000, total_war_reachability_boundary__dropping_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(twrb_drop_tr_t2000, observed).
narrative_ontology:measurement(twrb_drop_tr_t2015, total_war_reachability_boundary__dropping_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(twrb_drop_tr_t2015, observed).
narrative_ontology:measurement(twrb_drop_tr_t2026, total_war_reachability_boundary__dropping_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(twrb_drop_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(twrb_drop_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.82).
narrative_ontology:measurement_basis(twrb_drop_be_t1945, observed).
narrative_ontology:measurement(twrb_drop_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.78).
narrative_ontology:measurement_basis(twrb_drop_be_t1962, observed).
narrative_ontology:measurement(twrb_drop_be_t1979, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1979, 0.72).
narrative_ontology:measurement_basis(twrb_drop_be_t1979, observed).
narrative_ontology:measurement(twrb_drop_be_t2000, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(twrb_drop_be_t2000, observed).
narrative_ontology:measurement(twrb_drop_be_t2015, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(twrb_drop_be_t2015, observed).
narrative_ontology:measurement(twrb_drop_be_t2026, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(twrb_drop_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(twrb_drop_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement_basis(twrb_drop_su_t1945, observed).
narrative_ontology:measurement(twrb_drop_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement_basis(twrb_drop_su_t1962, observed).
narrative_ontology:measurement(twrb_drop_su_t1979, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1979, 0.71).
narrative_ontology:measurement_basis(twrb_drop_su_t1979, observed).
narrative_ontology:measurement(twrb_drop_su_t2000, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement_basis(twrb_drop_su_t2000, observed).
narrative_ontology:measurement(twrb_drop_su_t2015, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(twrb_drop_su_t2015, observed).
narrative_ontology:measurement(twrb_drop_su_t2026, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(twrb_drop_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.18).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'total_war_reachability_boundary.' The dropping_reading characterizes total war as persistent but probabilistically dropping due to deterrence equilibrium (tangled_rope type). Sibling readings characterize it as either structurally unreachable (contraction_reading, mountain-like) or as an atrophied but reversible capability (contingent_reachability_reading, piton). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and classifications. They are linked via this network field to enable contention analysis across the kernel's reading space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerless, 0.92).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
