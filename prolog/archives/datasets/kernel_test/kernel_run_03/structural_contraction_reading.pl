% ============================================================================
% CONSTRAINT STORY: structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_contraction_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_contraction_reading
 *   human_readable: Nuclear Impossibility: Structural Contraction of War as Rational Option
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   Nuclear weapons have created an unprecedented structural constraint: the
 *   impossibility of rational victory through war at the nuclear threshold.
 *   Once two or more actors possess credible nuclear arsenals, the
 *   traditional strategic calculus of war—victory achievable through superior
 *   force, strategy, or attrition—becomes logically irrational. Mutual
 *   annihilation is not a rare catastrophic outcome but a certainty if
 *   escalation reaches the nuclear level. This constraint is one reading of
 *   the contested nuclear-impossibility kernel, specifically the
 *   structural-contraction reading: war does not cease to exist as a
 *   category, but it contracts out of the reachable set for rational state
 *   actors. The constraint extracts from those seeking traditional strategic
 *   victory (rational war option is gone) and structurally benefits those who
 *   control the deterrence narrative (institutional actors, military
 *   establishments, arms control regimes). The extractiveness has increased
 *   over the post-1945 interval as nuclear arsenals have proliferated and
 *   decision-makers have internalized that escalation beyond conventional
 *   threshold guarantees mutual destruction. Theater ratio has also increased
 *   as strategic posturing, crisis management, and deterrence signaling have
 *   become more elaborate—the apparent rationality of nuclear deterrence
 *   requires continuous performative reinforcement (war-game exercises,
 *   strategic doctrine reviews, command continuity drills). The constraint is
 *   classified as Snare from most perspectives because the option space has
 *   contractually eliminated traditional war rationality without providing an
 *   alternative rational mechanism for strategic coercion.
 *
 * KEY AGENTS:
 *   - Strategic State (powerless in context of nuclear threshold): Trapped by the structural impossibility — cannot pursue traditional war objectives rationally; bears the extraction of losing a fundamental strategic option.
 *   - Nuclear-Armed State (organized): Constrained by the same impossibility but has organizational capacity to navigate the constraint through proxy wars, diplomatic leverage, and deterrence posturing; experiences lower effective extraction through arbitrage options (deterrence maintenance, strategic partnership, regional dominance).
 *   - Deterrence System Institutions (institutional/arbitrage): Military establishments, strategic think tanks, arms control bureaucracies — benefit from the constraint because it justifies continuous nuclear modernization, strategic analysis, and deterrence expenditures. Experience the constraint as legitimate coordination mechanism.
 *   - Non-Nuclear States (powerless/trapped): Forced into permanent subordinate strategic position; lack rational recourse to challenge nuclear-armed powers through traditional military means. Maximum extraction — trapped in a unipolar security order.
 *   - Analytical Observer (civilizational): Sees both the coordination benefit (prevents major-power war) and the extraction mechanism (concentrates geopolitical power). The constraint is simultaneously genuine deterrent and mechanism of hegemonic control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_contraction_reading, 0.68).
domain_priors:suppression_score(structural_contraction_reading, 0.72).
domain_priors:theater_ratio(structural_contraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_contraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_contraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(structural_contraction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_contraction_reading, snare).
narrative_ontology:human_readable(structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of War as Rational Option").
narrative_ontology:topic_domain(structural_contraction_reading, "strategic_studies/nuclear_deterrence/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_contraction_reading, '225daf9b-5abd-473f-8aeb-6d0dc309e1a9').
narrative_ontology:cs_created_at('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', '').
narrative_ontology:cs_kernel_codification('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', distributed).
narrative_ontology:cs_authority_grounding('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', extraction).
narrative_ontology:cs_interpretation_layer_present('225daf9b-5abd-473f-8aeb-6d0dc309e1a9').
narrative_ontology:cs_kernel_id(structural_contraction_reading, nuclear_impossibility_kernel).
narrative_ontology:cs_reading_relation('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', rational_dropout_reading, coexists_with).
narrative_ontology:cs_reading_relation('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', foundational, war_exits_reachable_set).
narrative_ontology:cs_axiom_status(war_exits_reachable_set, holdable).
narrative_ontology:cs_axiom_grounding('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', war_exits_reachable_set, empirically_contingent).
narrative_ontology:cs_axiom('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', foundational, proxy_wars_are_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_wars_are_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', proxy_wars_are_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', classical_war_rationality).
narrative_ontology:cs_drift_state('225daf9b-5abd-473f-8aeb-6d0dc309e1a9', post_nuclear_arsenal_proliferation, gap(axiom_overriding, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(structural_contraction_reading, strategic_statecraft).
narrative_ontology:constraint_victim(structural_contraction_reading, rational_war_option).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONAL ACTOR (SNARE) — A strategist seeking rational victory through war faces an immutable structural constraint: mutual annihilation is guaranteed if escalation reaches the nuclear threshold. The option space has contracted from 'war with victory possible' to 'war with mutual destruction certain.' No exit exists from this constraint without abandoning war itself. Maximal experienced extraction — rational agency is stripped of its traditional recourse.
constraint_indexing:constraint_classification(structural_contraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NUCLEAR-ARMED STATE (SNARE) — Possesses organizational capacity to act, but the structural constraint remains: war cannot be won if escalation reaches nuclear threshold. Exit options exist only through disarmament (high-cost, reversible by rivals) or conflict avoidance (reduces strategic utility of nuclear capability itself). The state is trapped between war as irrational and peace as unstable equilibrium maintained by mutual terror.
constraint_indexing:constraint_classification(structural_contraction_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DETERRENCE SYSTEM INSTITUTIONS (ROPE) — Military establishments, strategic think tanks, arms control regimes, and nuclear command structures all benefit from and maintain the constraint. The impossibility of nuclear victory becomes the justification for nuclear possession, continuous modernization, and strategic posturing. The constraint coordinates deterrence logic: 'mutual annihilation ensures peace through credible threat.' The system experiences this as legitimate coordination, not extraction.
constraint_indexing:constraint_classification(structural_contraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint exhibits both coordination and extraction. Coordination function: prevents major-power war through credible deterrence (genuine collective benefit). Extraction function: concentrates geopolitical power in nuclear-armed states and forces non-nuclear states into subordinate positions. The constraint is hybrid — neither pure law nor pure oppression, but a stabilizing mechanism that simultaneously amplifies inequality.
constraint_indexing:constraint_classification(structural_contraction_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_contraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_contraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_contraction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts the traditional rational option for war—the possibility of victory through superior force or strategy. This loss is irreversible once nuclear arsenals exist and are credibly deployed. The extractiveness has increased over the interval (from 0.45 at nuclear threshold 1945 to 0.68 contemporary) as more states acquired arsenals and strategic doctrine matured. Suppression (0.72): High. The constraint is sustained by physical fact (nuclear physics), institutional enforcement (command-and-control systems, security infrastructure), and cognitive capture (strategic rationality is now defined within the constraint). States cannot exit by acquiring nuclear capability—they are already trapped. They cannot exit through disarmament—rivals retain capability and could betray agreements. They cannot exit through conventional military strategy—nuclear threshold makes conventional victory impossible to guarantee. Theater ratio (0.58): Moderate-high. Strategic posturing and deterrence signaling are substantial but not total. The underlying physics is real (mutual annihilation is genuine), but much of the operational deterrence apparatus is performative—war games, threat assessments, strategic doctrine that claims rationality within an inherently irrational condition. The constraint is not pure theater (the threat is credible) but requires constant theatrical maintenance to sustain belief in deterrence stability.
 *
 * PERSPECTIVAL GAP:
 *   The rational actor (powerless/trapped) experiences maximal extraction — traditional war rationality has been eliminated. The nuclear-armed state (organized/constrained) experiences lower extraction through diplomatic leverage and proxy-war arbitrage. The deterrence institution (institutional/arbitrage) experiences no extraction but rather coordination benefit — the constraint justifies their existence and mission. The analytical observer sees the constraint as simultaneously a stabilizing coordination mechanism (prevents major-power war through credible mutual deterrence) and an extraction mechanism (concentrates geopolitical power in nuclear-armed states and forces non-nuclear states into subordination). The perspectival gap reveals that 'impossibility of nuclear victory' is not a neutral constraint but a mechanism that redistributes strategic power away from traditional military logic and toward deterrence management, diplomatic pressure, and hegemonic control by nuclear-armed powers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship of each agent to the constraint. The rational actor seeking war victory has d approaching 1.0 (full target — the constraint extracts the war option entirely). The nuclear-armed state has lower d (beneficiary of deterrence stability + arbitrage options through proxy war and diplomacy, but also victim of constrained strategic options). The deterrence institution has d approaching 0.0 (full beneficiary — the constraint is their raison d'être). The non-nuclear state has d approaching 1.0 (full target — lacks nuclear option and therefore lacks rational recourse to nuclear-armed powers). The analytical observer's d is computed from observer position (0.72 approximate) and reflects the mixed coordination-extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through kernel reading specification. The structural-contraction reading makes a specific claim: war has exited the reachable set for rational actors. This is distinct from the rational-dropout reading (actors choose to avoid war because doing so is rational) and the credibility-paradox reading (deterrence is self-defeating because the threat to use nuclear weapons is not credible). All three readings are live for different methodological and normative camps in strategic studies. The structural-contraction reading is NOT foreclosed by the others — they coexist as competing explanations of post-1945 peace among great powers. The mandatrophy is resolved by specifying WHICH reading is being modeled: this constraint models structural contraction, not rational dropout or credibility paradox. Each reading has its own extractiveness, its own victim/beneficiary structure, and its own measurement trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_empirical_impossibility,
    'Is the impossibility of nuclear victory a structural logical necessity or an empirical contingency dependent on command-and-control systems, escalation dynamics, or decision-maker rationality?',
    'Counterfactual analysis: what would change the impossibility? (a) If nothing changes it regardless of context = structural; (b) if command failure, miscalculation, or irrational decision-maker could change it = empirical contingency. Analysis of near-miss incidents and decision-making under nuclear crisis conditions.',
    'If structural: constraint is mountain-adjacent (immutable once nuclear arsenals exist). If empirical: constraint is contingent on institutional maintenance and could degrade if decision-making systems fail. Classification shifts from snare (contingent institutional trap) to mountain (structural law of nuclear physics/logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_empirical_impossibility, conceptual, 'Whether nuclear impossibility is logical necessity or empirical contingency').

omega_variable(
    proxy_war_substitution_mechanism,
    'Do proxy wars and conventional conflicts represent genuine continuation of rational strategy below the nuclear threshold, or are they coercive extraction mechanisms substituting for direct war that would be irrational?',
    'Structural analysis of proxy conflict outcomes vs direct war outcomes. If proxy wars achieve strategic objectives efficiently: continuation logic holds. If proxy wars systemically fail to achieve stated strategic objectives but persist anyway: substitution trap logic holds (snare from strategic perspective). Comparative analysis: post-1945 interstate conflicts vs pre-1945 equivalent scenarios.',
    'If continuation: structural contraction is partial; rational war option survives below nuclear threshold. If substitution: structural contraction is near-total; the appearance of rational proxy war conceals that direct war has become irrational and all remaining conflicts are coercive extraction. This determines whether snare classification holds or degrades to piton (degraded rationality theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_war_substitution_mechanism, empirical, 'Whether proxy wars are rational continuation or coercive substitution').

omega_variable(
    reading_alternative_contraction_hypotheses,
    'Is the impossibility of nuclear victory the primary causal mechanism behind observed post-1945 strategic contraction, or are other mechanisms (economic interdependence, institutional linkage, information asymmetry, identity alignment) doing more causal work?',
    'Counterfactual: if nuclear weapons were somehow magically removed from the post-1945 world while keeping all other conditions (institutions, economics, communication technology, ideology) identical, would great-power war resume? Comparative historical analysis of decision-making in Cuban Missile Crisis, Kargil War, and other nuclear-adjacent crises vs. pre-nuclear great-power crises.',
    'If nuclear impossibility is primary causal mechanism: snare classification of structural contraction is confirmed. If other mechanisms dominate: the nuclear constraint may be rationalization (false summit) of structural changes driven by economic/institutional factors, degrading this reading to piton (theater of deterrence justifying institutional inertia). This omega determines whether the constraint is causally real or epiphenomenal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_alternative_contraction_hypotheses, empirical, 'Nuclear impossibility as primary vs. secondary causal mechanism for post-1945 strategic shift').

omega_variable(
    kernel_reading_under_determination,
    'Does the structural contraction reading fundamentally foreclose the rational-dropout reading (actors rationally select into peace), or do they coexist as different framings of the same phenomenon?',
    'Logical analysis: structural contraction (war exits reachable set) vs rational dropout (actors choose to exit war). Can both statements be true? If yes: coexists_with. If no: identify which premise forecloses the other. If ambiguous: emit conceptual omega at the committer level documenting the reading under-determination.',
    'If forecloses: sibling reading is incoherent given this reading''s commitments. If coexists: both readings are live options for different causal explanations. Determines the reading_relations classification in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Logical compatibility of structural contraction and rational dropout readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_contraction_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(struct_contract_theater_t0, structural_contraction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(struct_contract_theater_t20, structural_contraction_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(struct_contract_theater_t75, structural_contraction_reading, theater_ratio, 75, 0.58).

% Extraction over time
narrative_ontology:measurement(struct_contract_extract_t0, structural_contraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(struct_contract_extract_t20, structural_contraction_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(struct_contract_extract_t75, structural_contraction_reading, base_extractiveness, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_contraction_reading, rational_dropout_reading).
narrative_ontology:affects_constraint(structural_contraction_reading, credibility_paradox_reading).
narrative_ontology:affects_constraint(structural_contraction_reading, proxy_war_rationality).
narrative_ontology:affects_constraint(structural_contraction_reading, nuclear_command_and_control_reliability).

% DUAL FORMULATION NOTE:
% The structural-contraction reading is upstream of both rational-dropout and credibility-paradox readings. If structural contraction is true (war exits reachable set), then rational-dropout is a lower-level mechanism (how actors navigate the contraction). If credibility-paradox is true (deterrence is unstable), then structural-contraction's claim of impossibility is questioned. This story links to proxy_war_rationality because the constraint's existence depends on proxy wars being genuine substitution (not continuation of rational war). Links to command_and_control because the impossibility depends on reliable deterrent posture—if C2 systems fail, the impossibility may not hold empirically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_contraction_reading, powerless, 0.98).
constraint_indexing:directionality_override(structural_contraction_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
