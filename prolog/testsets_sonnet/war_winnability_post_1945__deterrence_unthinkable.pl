% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear-Age Categorical Unwinnability of Great-Power Total War
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel about whether
 *   nuclear weapons changed the reachable space of great-power war outcomes.
 *   Under this reading, thermonuclear capability categorically removed
 *   'victory' from the set of coherent strategic objectives for total war
 *   between nuclear peers — not merely constraining it (the
 *   countervailing_thinkable sibling) or pushing it out of public discourse
 *   while leaving it operationally planned (the rhetorical_contraction
 *   sibling), but eliminating it from the reachable operational space
 *   entirely. Military establishments retain enormous institutional mass
 *   built around a mission whose terminal state (decisive victory in total
 *   war against a peer) no longer exists as a coherent target; civilian
 *   populations benefit from a structural insulation against total
 *   annihilation that no doctrine or treaty produced but that the underlying
 *   physics enforces regardless of institutional preference.
 *
 * KEY AGENTS:
 *   - civilian_populations: primary beneficiary (powerless/trapped) — insulated from total war outcomes by a fact outside their control
 *   - military_establishments: primary victim of mission incoherence (institutional/constrained) — retain institutional mass without a coherent victory-oriented terminal state
 *   - nuclear_weapon_states: agenda-setters who created and administer the underlying arsenals (institutional/trapped) — bound by the same structural fact they produced
 *   - counterforce_theorists: excluded voice arguing the foreclosure is not categorical
 *   - arms_control_analysts: analytical observers adjudicating between kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.31).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.42).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.31).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear-Age Categorical Unwinnability of Great-Power Total War").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'd4f0460a-9176-46f7-9ff1-e994ecb15066').
narrative_ontology:cs_kernel_codification('d4f0460a-9176-46f7-9ff1-e994ecb15066', distributed).
narrative_ontology:cs_authority_grounding('d4f0460a-9176-46f7-9ff1-e994ecb15066', distributed).
narrative_ontology:cs_reading_relation('d4f0460a-9176-46f7-9ff1-e994ecb15066', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('d4f0460a-9176-46f7-9ff1-e994ecb15066', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('d4f0460a-9176-46f7-9ff1-e994ecb15066', foundational, victory_categorically_excluded_from_reachable_space).
narrative_ontology:cs_axiom_status(victory_categorically_excluded_from_reachable_space, holdable).
narrative_ontology:cs_axiom_grounding('d4f0460a-9176-46f7-9ff1-e994ecb15066', victory_categorically_excluded_from_reachable_space, empirically_contingent).
narrative_ontology:cs_axiom('d4f0460a-9176-46f7-9ff1-e994ecb15066', secondary, war_prevention_supersedes_war_winning_as_strategic_terminus).
narrative_ontology:cs_axiom_status(war_prevention_supersedes_war_winning_as_strategic_terminus, holdable).
narrative_ontology:cs_axiom_grounding('d4f0460a-9176-46f7-9ff1-e994ecb15066', war_prevention_supersedes_war_winning_as_strategic_terminus, instrumental).
narrative_ontology:cs_reference_frame('d4f0460a-9176-46f7-9ff1-e994ecb15066', pre_nuclear_clausewitzian_war_theory).
narrative_ontology:cs_drift_state('d4f0460a-9176-46f7-9ff1-e994ecb15066', post_cuban_missile_crisis_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d4f0460a-9176-46f7-9ff1-e994ecb15066', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, nuclear_taboo_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have no voice in strategic doctrine but are the direct beneficiaries of a structural fact that removes total great-power war from the set of things that can rationally be planned toward victory. Their survival is contingent on the physics holding, not on any institution's goodwill.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Built around the professional mission of winning wars through decisive engagement and escalation dominance. The categorical foreclosure of victory in great-power total war does not remove their institutions but hollows the coherence of their founding purpose against a peer nuclear adversary, forcing doctrinal reinvention (deterrence maintenance, limited war, proxy conflict) as substitutes for a mission that no longer has a coherent terminal state.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, constrained, national).

% Possess the arsenals that instantiate the constraint and administer doctrines (declaratory policy, arms control, command-and-control) that manage its consequences, but cannot exit the structural fact they created — they are as bound by the unwinnability as anyone else, and their planning apparatus must continuously reconcile prestige and deterrence signaling with the underlying impossibility of victory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, nuclear_weapon_states, payer).

% Benefit from the fact that the great powers cannot rationally fight each other to a decisive conclusion, which caps the scale of catastrophic war they could be drawn into, while bearing none of the doctrinal cost of maintaining the arsenals that produce this effect.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states, beneficiary,
    moderate, generational, constrained, global).

% Study the strategic-stability literature, verify the empirical record of crisis behavior (Cuban Missile Crisis, near-misses, doctrine evolution) and adjudicate between rival readings of what nuclear weapons actually did to the concept of victory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_analysts, observer,
    analytical, generational, analytical, global).

% Argue from within military and strategic-studies institutions that limited nuclear victory remains achievable through counterforce targeting and damage limitation; this reading treats their claim as foreclosed rather than merely contested, so their position is not represented as a live possibility inside this constraint's own frame.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_theorists, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not an arrangement solving a coordination problem but a claimed structural fact about what physical destructive capacity does to the rational pursuit of decisive military victory between peer nuclear powers.
% TRANSFER_FUNCTION: Moves strategic planning purpose away from victory-seeking and toward war-avoidance; correspondingly moves institutional coherence away from military establishments (whose founding mission presumed winnable wars) toward civilian survival as the operative value.
% ABSENT_VOICES: Counterforce theorists and limited-nuclear-war planners are structurally excluded from this reading's frame — they would argue that damage-limitation and escalation-control strategies preserve a coherent concept of relative advantage even under nuclear conditions, but this reading treats that claim as foreclosed rather than live.
% DISAPPEARANCE_RATIONALE: If the categorical unwinnability were false (or if a technological development such as reliable missile defense restored decisive victory to the reachable space), military establishments' doctrines of decisive engagement would regain coherence, arms-race dynamics would intensify around achieving first-strike or damage-limitation advantage, and civilian populations would lose the structural insulation the constraint currently provides.
% FOUNDING_PROBLEM: The advent of thermonuclear weapons and reliable delivery systems created a situation in which total war between possessors could produce mutual annihilation regardless of who 'won' the initial exchange, making the traditional military calculus of relative cost-benefit for total war incoherent.
% FOUNDING_PROBLEM_CORROBORATION: Independent nuclear strategists (Schelling, Jervis, Waltz) and multiple declassified crisis-management archives attest the founding problem is real and unresolved from outside any single military establishment's institutional interest; some counterforce-oriented planners within military and civilian defense-analysis institutions dispute that winnability is categorically foreclosed rather than merely degraded, which is why status is marked contested rather than settled.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.31) because this reading is fundamentally about an operational-space contraction, not a rent-extraction mechanism — there is no party collecting a transfer from the constraint's operation in the ordinary sense; the 'cost' borne by military establishments is mission incoherence rather than material extraction. Suppression (0.42) reflects the real institutional and doctrinal pressure exerted against strategic planning that pretends victory remains achievable — war colleges, defense budgets, and doctrine documents that ignore the constraint are corrected by crisis behavior and career consequences. Accessibility collapse is high (0.81): once the destructive yield and delivery reliability of thermonuclear arsenals is understood, the alternative of 'total war with a winning side' becomes very difficult to construct as a coherent plan, which is precisely the mountain-like signature this reading claims. Resistance is moderate (0.55) because counterforce and damage-limitation theorists persistently resist the categorical framing from within military and strategic-studies institutions — this is exactly the sibling reading (countervailing_thinkable) that this constraint's own frame treats as foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and non-nuclear states are coded as beneficiaries: the constraint (if true) removes them from the reachable set of total-war casualties without requiring any action or payment on their part — near-full subsidy, low d. Military establishments are coded as victims of mission incoherence: they bear the cost not through material extraction but through structural displacement of the professional purpose their institutions were built around — this pushes their directionality toward the target end despite their institutional power, because the constraint's operation degrades something specific and identifiable (coherent doctrine) that they cannot exit from (their exit_options are constrained, not mobile — an institution cannot simply stop being a military establishment). Nuclear weapon states occupy an unusual position: they are simultaneously agenda-setters (administering doctrine, arms control, signaling) and payers (bound by the same physical fact, unable to exit the mutual vulnerability they created), which is why they carry a secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem status is marked contested rather than dead precisely to avoid mislabeling this as either pure mandatrophy (the mission is dead, the institution just persists) or pure ongoing coordination. Military establishments have not become obsolete — they perform real functions (deterrence maintenance, alliance reassurance, conventional and sub-nuclear operations) — but their founding total-war-winning mission has lost its coherent terminal state under this reading, producing a genuine mismatch between doctrinal self-conception and structural reality that the disappearance_verdict (world_rearranges) and founding_problem_status (contested) jointly surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_deterrence_doctrine,
    'Is the categorical unwinnability of great-power total war a genuine structural fact about thermonuclear physics and human survivability, or is it a doctrine constructed and maintained by nuclear-weapon-state institutions whose bureaucratic and political interests are served by the perception of foreclosure?',
    'Examine whether declared beneficiaries (civilian populations, non-nuclear states) would still be described as benefiting under a fully independent physical/actuarial analysis of nuclear exchange outcomes, decoupled from any nuclear-weapon-state''s declaratory policy; cross-check against counterforce theorists'' technical claims about damage limitation.',
    'If the unwinnability is substantially a constructed doctrine serving nuclear-weapon-state bureaucratic interests (arsenal maintenance budgets, prestige, alliance leverage) rather than an inescapable physical fact, this constraint would need re-classification away from mountain toward a tangled_rope or snare reading, closer to the countervailing_thinkable or rhetorical_contraction siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_deterrence_doctrine, conceptual, 'Whether the categorical-foreclosure claim is a discovered physical/strategic fact or an institutionally constructed and self-serving doctrine.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable evidence would distinguish this reading (operational contraction) from the rhetorical_contraction sibling (discursive taboo without operational change) — i.e., do actual war plans, targeting doctrines, and command-and-control postures reflect genuine abandonment of victory-seeking, or do they retain counterforce and damage-limitation planning beneath declaratory rhetoric?',
    'Declassified targeting doctrine, war-gaming records, and force-posture analysis across nuclear-weapon-state militaries over the interval; compare declaratory statements (''no one can win a nuclear war'') against actual procurement and doctrinal investment in counterforce and damage-limitation capabilities.',
    'If declassified planning shows sustained investment in counterforce and damage-limitation postures despite declaratory unwinnability rhetoric, the correct reading of the underlying kernel is closer to rhetorical_contraction, and this story''s claimed_type and stakeholder structure would need substantial revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether operational planning evidence supports genuine operational contraction versus rhetorical contraction with continued counterforce planning underneath.').

omega_variable(
    military_establishment_adaptation_vs_incoherence,
    'Have military establishments genuinely lost mission coherence under this constraint, or have they successfully substituted deterrence-maintenance, alliance-management, and limited/conventional warfighting as a fully coherent replacement mission that resolves the apparent incoherence?',
    'Institutional analysis of defense doctrine documents, officer career-path incentives, and budget allocation trends to determine whether the substitute missions (deterrence, alliance reassurance) function as a genuinely coherent replacement terminal state or as a patchwork masking unresolved purpose displacement.',
    'If deterrence-maintenance has become a fully coherent substitute mission, the victim classification of military_establishments weakens substantially and the constraint''s extraction/incoherence framing should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_establishment_adaptation_vs_incoherence, empirical, 'Whether military institutions have successfully re-coherenced around deterrence rather than suffering genuine ongoing mission incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.14).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1975, 0.19).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.23).
narrative_ontology:measurement(war__tr_t2008, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2008, 0.26).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1975, 0.27).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.29).
narrative_ontology:measurement(war__be_t2008, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(war_winnability_post_1945__deterrence_unthinkable, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the war_winnability_post_1945 kernel, each instantiated as a separate ε-invariant story per the decomposition rule: deterrence_unthinkable (this story, claiming categorical operational contraction, Mountain-leaning), countervailing_thinkable (claiming limited nuclear victory remains achievable via counterforce, likely Tangled Rope or contested Rope given active military institutional interest in the claim), and rhetorical_contraction (claiming discursive taboo without operational change, likely Piton or Scaffold given the gap between declaratory rhetoric and retained war-planning function). The three share stakeholder categories (military establishments, civilian populations, nuclear weapon states) but assign them different roles and different ε values because each reading makes a structurally distinct empirical claim about what nuclear weapons actually did to strategic planning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
