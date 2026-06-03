% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: Article 27 Veto as War-Prevention Coordination Mechanism
 *   domain: international_relations/constitutional_law/security
 *
 * SUMMARY:
 *   Article 27 of the UN Charter establishes the veto power of the five
 *   permanent Security Council members (P5: USA, USSR/Russia, UK, France,
 *   China), requiring unanimity for all substantive Council resolutions. This
 *   constraint story instantiates the COORDINATION READING of the veto
 *   mechanism: the veto is understood as a necessary institutional device
 *   that prevents the UN Security Council from compelling any nuclear-armed
 *   state into military action against its strategic interests, thereby
 *   eliminating a causal pathway to great-power war. Under this reading, the
 *   veto solves a genuine collective-action problem — without it, the
 *   Security Council's majority-rule authority could force P5 states into
 *   unwanted military entanglement, triggering exit from the system (and
 *   system collapse) or great-power conflict. The coordination reading
 *   interprets the veto as functionally solving the problem of how to bind
 *   independent sovereigns into a collective security framework without
 *   creating perverse incentives that destabilize the system itself. Base
 *   extractiveness (0.12) reflects that the mechanism is genuinely
 *   coordination-dominant: no agent extracts asymmetric benefit; all P5
 *   states benefit equally from the prevention of compelled military action,
 *   and non-P5 states benefit from the reduced probability of great-power war
 *   that would otherwise engulf them. Theater ratio (0.15) is low — the
 *   veto's function is transparent and direct, with minimal performative
 *   overlay. This reading coexists with two sibling readings: the
 *   oligopoly_reading (ε=0.68, snare), which frames the veto as structural
 *   entrenchment of geopolitical privilege and blocking of institutional
 *   evolution; and the sovereignty_reading, which frames the veto as an
 *   instantiation of Westphalian principle applied to nuclear-armed states.
 *   The three readings partition the contested legitimacy claim grounded in
 *   the Article 27 kernel.
 *
 * KEY AGENTS:
 *   - P5 States (USA, Russia, UK, France, China): Institutional beneficiaries with arbitrage exit — coordinate to prevent compelled military action; benefit from structure that protects each from majority-vote coercion
 *   - Non-P5 Security Council Members: Organized beneficiaries with constrained exit — accept veto structure because alternative (no veto → great-power war risk) is worse for their security
 *   - International System Stability: Abstract beneficiary — the veto prevents a specific causal pathway to great-power war, stabilizing the entire post-1945 security architecture
 *   - Game-Theoretic Observer: Analytical position — sees the veto as solution to prisoners' dilemma in binding independent nuclear-armed actors into collective framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.12).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.08).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "Article 27 Veto as War-Prevention Coordination Mechanism").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/constitutional_law/security").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '41aeb0e5-4ece-4071-8da1-0c190cc656c6').
narrative_ontology:cs_kernel_codification('41aeb0e5-4ece-4071-8da1-0c190cc656c6', formalized).
narrative_ontology:cs_authority_grounding('41aeb0e5-4ece-4071-8da1-0c190cc656c6', lineage).
narrative_ontology:cs_interpretation_layer_present('41aeb0e5-4ece-4071-8da1-0c190cc656c6').
narrative_ontology:cs_reading_relation('41aeb0e5-4ece-4071-8da1-0c190cc656c6', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('41aeb0e5-4ece-4071-8da1-0c190cc656c6', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('41aeb0e5-4ece-4071-8da1-0c190cc656c6', foundational, veto_prevents_compelled_military_action).
narrative_ontology:cs_axiom_status(veto_prevents_compelled_military_action, holdable).
narrative_ontology:cs_axiom_grounding('41aeb0e5-4ece-4071-8da1-0c190cc656c6', veto_prevents_compelled_military_action, empirically_contingent).
narrative_ontology:cs_axiom('41aeb0e5-4ece-4071-8da1-0c190cc656c6', foundational, great_power_war_risk_eliminated_without_veto).
narrative_ontology:cs_axiom_status(great_power_war_risk_eliminated_without_veto, holdable).
narrative_ontology:cs_axiom_grounding('41aeb0e5-4ece-4071-8da1-0c190cc656c6', great_power_war_risk_eliminated_without_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('41aeb0e5-4ece-4071-8da1-0c190cc656c6', security_council_unanimity_requirement).
narrative_ontology:cs_drift_state('41aeb0e5-4ece-4071-8da1-0c190cc656c6', contemporary_geopolitical_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41aeb0e5-4ece-4071-8da1-0c190cc656c6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, all_security_council_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, nuclear_weapons_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: P5 STATE / INSTITUTIONAL COORDINATION (ROPE) — The veto is a genuine coordination mechanism solving a real collective-action problem: without unanimity gate, Security Council resolutions could compel nuclear-armed states into military action against their strategic interests, triggering great-power war. The veto gives each P5 state the power to prevent their own entrapment. No extraction occurs — all P5 states benefit equally from the structure that prevents any of them from being forced into unwanted war. The mechanism is low-theater: it functions exactly as designed.
constraint_indexing:constraint_classification(article_27_veto_power__coordination_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: RATIONAL SECURITY STRATEGIST (ROPE) — From a pure security analysis, the veto is an elegant coordination solution. It prevents the scenario where a P5 state with independent nuclear deterrent capacity is locked into military action by majority vote. The alternative — no veto — creates a prisoners' dilemma where each P5 state would rationally defect from the UN system rather than surrender veto power, making the system collapse. The veto preserves the system's existence and legitimacy. Low extractiveness because the mechanism produces genuine surplus (avoided great-power conflict) without asymmetric distribution.
constraint_indexing:constraint_classification(article_27_veto_power__coordination_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-ALIGNED STATE COALITION (ROPE) — Non-P5 states accept the veto as a coordination mechanism because the alternative is worse: without it, great-power war over UN-mandated interventions becomes likely, and such wars drag in coalitions of smaller states. The veto, by preventing forced P5 participation, prevents the causal chain that leads to great-power conflict and attendant collateral. Non-aligned states are constrained (they have no veto), but the coordination function benefits them by reducing the probability of great-power war that would engulf their own security.
constraint_indexing:constraint_classification(article_27_veto_power__coordination_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GAME-THEORETIC ANALYST / COORDINATION FRAME (ROPE) — From a formal game theory standpoint, the veto solves the core collective-action problem of international governance: how to bind independent actors into a framework without creating perverse incentives that trigger exit (great-power withdrawal). The veto is a constitutional unanimity rule that prevents the majority from creating Pareto-dominated outcomes for nuclear-armed minorities. Theater is low — the mechanism's function is transparent and direct.
constraint_indexing:constraint_classification(article_27_veto_power__coordination_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. Under the coordination reading, no agent extracts at the expense of others. All P5 states benefit from the unanimity requirement that prevents any of them from being locked into military action by majority vote. The benefit is symmetric — each P5 state receives equivalent protection. Non-P5 states also benefit (reduced great-power war probability), though they do not have veto power themselves. The small non-zero value (0.12 rather than 0.00) reflects a minor asymmetry: P5 states do derive the benefit of privileged status and permanent seat (not available to other states), but this status flows from the underlying security structure, not from the veto mechanism itself. The veto mechanism is coordination-pure. Suppression (0.08): Very low. The mechanism operates through explicit constitutional rule (Article 27), not through suppression of alternatives. Non-P5 states are constrained by their lack of veto power, but this is a structural feature, not a suppression mechanism. Theater ratio (0.15): Low. The veto's function is transparent and direct — it prevents Security Council resolutions from passing without unanimity. This function is exactly as designed and publicly articulated. The mechanism requires minimal performative overlay.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading (coordination) and its siblings is structural. The coordination_reading emphasizes the war-prevention function: the veto prevents a specific causal pathway (Security Council forced military action → great-power strategic crisis → great-power war). The oligopoly_reading emphasizes the entrenchment function: the veto blocks institutional evolution, prevents normative expansion, and protects P5 privilege from redistribution. The sovereignty_reading emphasizes the consent principle: the veto instantiates Westphalian sovereignty (no binding without consent) applied to states with global-reach enforcement capacity. All three readings operate on the same empirical object (Article 27) but emphasize different causal chains. The readings are not empirically contradictory — the veto does prevent compelled military action, does entrench P5 privilege, and does instantiate consent principles. The disagreement is about which function is PRIMARY and which are secondary or incidental. The coordination_reading treats entrenchment as a side effect of the necessary war-prevention mechanism; the oligopoly_reading treats war-prevention as a post-hoc rationalization for geopolitical entrenchment; the sovereignty_reading treats both as instantiations of a deeper Westphalian principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's position as beneficiary or target in the coordination structure. P5 states are institutional beneficiaries with arbitrage exit — they chose to join the system and retain veto power as the condition for membership. Their d value is low (~0.10-0.15), producing minimal effective extractiveness. Non-P5 states are beneficiaries of the reduced great-power war probability, though they are constrained (lack veto). Their d value is moderate (~0.45-0.50), reflecting that they benefit from coordination but lack the privileged position. All perspectives converge on rope classification because no agent is being extracted from — the mechanism solves a collective-action problem that all agents prefer solved.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_counterfactual_war_risk,
    'Would elimination of the veto increase the probability of great-power war through Security Council-mandated military action compulsion?',
    'Historical counterfactual analysis: modeling scenarios where Security Council resolutions would have passed without veto; game-theoretic analysis of P5 exit incentives under majority-rule alternatives; strategic documents indicating veto as constraint on great-power action.',
    'If true: veto is genuine coordination mechanism (supports rope classification). If false or indeterminate: veto may be post-hoc rationalization for geopolitical entrenchment (supports sibling oligopoly_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_counterfactual_war_risk, empirical, 'Whether veto elimination would increase great-power war probability').

omega_variable(
    unanimity_necessity_threshold,
    'Is unanimity among five independent nuclear-armed states the minimal institutional requirement to prevent compelled great-power war, or are there alternative governance structures (weighted voting, subset veto, conditional exit rights) that would achieve equivalent war-prevention with lower gridlock?',
    'Design of alternative institutional structures preserving war-prevention while enabling greater-than-veto decisiveness; analysis of historical institutional evolution in other contexts (EU, ASEAN, African Union) showing solutions to similar problems without unanimity gates.',
    'If unanimity is truly minimal: rope reading is correct and no institutional improvement is possible. If alternatives exist: the ''necessity'' framing may be rhetorical cover for preference for vetoed status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_necessity_threshold, conceptual, 'Whether unanimity is minimal for war-prevention or rhetorical cover').

omega_variable(
    reading_boundary_oligopoly_vs_coordination,
    'Where is the boundary between legitimate coordination (preventing compulsion into unwanted war) and extractive oligopoly (preserving geopolitical privilege)? Does the veto''s war-prevention function analytically separate from its entrenchment function, or are they structurally inseparable?',
    'Examine veto usage patterns: count votes where veto was deployed to prevent compelled military action vs votes where veto was used to block institutional reform, humanitarian intervention, or normative expansion. Analyze which scenarios the coordination_reading predicts (war-prevention blocks) vs the oligopoly_reading predicts (entrenchment blocks). If the veto blocks military action at rate ~0% but blocks governance reform at rate ~30%, they are distinct mechanisms.',
    'If separable: both readings are live and coexist. If inseparable: readings are foreclosing each other — the veto cannot simultaneously be purely coordinating and purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_oligopoly_vs_coordination, empirical, 'Separation of coordination vs oligopolistic functions in veto usage').

omega_variable(
    committer_frame_reading_contest,
    'This constraint is one reading of a contested kernel (Article 27 veto power). The coordination_reading frames the veto as a war-prevention mechanism; the oligopoly_reading frames it as geopolitical entrenchment; the sovereignty_reading frames it as Westphalian principle. Are these readings held by different institutional actors (legitimacy claim competition), or are they alternative framings available to a single actor (frame-switching)?',
    'Discourse analysis: which actors invoke which reading in which contexts? Do P5 states shift between coordination framing (when defending veto) and sovereignty framing (when justifying non-compliance)? Do critics use oligopoly framing? Institutional position determines which reading party holds.',
    'If readings align with actor positions (P5 uses coordination, non-P5 uses oligopoly, international law scholars use sovereignty): readings coexist via actor stratification. If readings are frame-switchable by single actors: readings influence each other (causal downstream relationship).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_contest, conceptual, 'Actor-stratification vs frame-switching for the three veto readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a27coord_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(a27coord_tr_t25, article_27_veto_power__coordination_reading, theater_ratio, 25, 0.13).
narrative_ontology:measurement(a27coord_tr_t50, article_27_veto_power__coordination_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(a27coord_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(a27coord_be_t25, article_27_veto_power__coordination_reading, base_extractiveness, 25, 0.1).
narrative_ontology:measurement(a27coord_be_t50, article_27_veto_power__coordination_reading, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, security_council_gridlock).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, great_power_war_probability).

% DUAL FORMULATION NOTE:
% Article 27 veto power is a contested kernel with three distinct readings: coordination_reading (this story, ε=0.12, rope), oligopoly_reading (sibling, ε=0.68, snare), and sovereignty_reading (sibling). The three readings are not alternative observables of the same constraint — they are structurally distinct constraints grounded in different legitimacy claim traditions. The coordination_reading emphasizes war-prevention function; the oligopoly_reading emphasizes entrenchment function; the sovereignty_reading emphasizes consent principle. All three operate on Article 27, but each instantiates a different constraint by emphasizing different causal chains. Link all three readings via network.affects_constraints to enable the compiler to recognize the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
