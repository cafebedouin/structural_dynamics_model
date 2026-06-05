% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Impossibility Kernel — Rational Dropout Reading
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   The rational dropout reading frames nuclear weapons as creating a
 *   rational-choice constraint: conventional victory through nuclear war
 *   remains structurally possible, but the expected costs exceed any
 *   conceivable benefit, rendering war irrational to initiate. War stays in
 *   the reachable choice set but is actively excluded from strategic
 *   consideration via cost-benefit calculation. This reading is ONE
 *   interpretation of the contested nuclear impossibility kernel. It differs
 *   from the credibility paradox reading (which argues that proving nuclear
 *   war is irrational undermines the threat's credibility, creating a logical
 *   trap) and the structural contraction reading (which argues that mutual
 *   annihilation is not merely irrational but physically inevitable, making
 *   war impossible rather than merely imprudent). The rational dropout
 *   reading preserves war as theoretically possible — this is crucial to its
 *   deterrent logic — while making it practically abandoned through rational
 *   choice. Extractiveness has increased over the 40-year interval (0.28 →
 *   0.38) as nuclear arsenals have accumulated, regional nuclear
 *   proliferation has advanced, and the constraint's suppressive force on
 *   non-nuclear states has intensified. Theater ratio has also risen (0.42 →
 *   0.62), reflecting the increasing performative character of deterrence
 *   postures as continuous nuclear readiness persists despite the
 *   near-universal acknowledgment that use would be catastrophic. Suppression
 *   has deepened (0.55 → 0.68) because the constraint has become more
 *   structural: non-nuclear states have fewer diplomatic tools to resist the
 *   dominance of nuclear-armed powers, and the constraint is now embedded in
 *   alliance structures, security guarantees, and NPT regimes that actively
 *   enforce the rational dropout logic.
 *
 * KEY AGENTS:
 *   - Non-Nuclear States: Primary victims (powerless/trapped) — face constraint but cannot exit through rational choice; subordinated by nuclear threat with no credible deterrent response
 *   - Nuclear-Armed States: Primary beneficiaries (institutional/arbitrage) — extract strategic advantage from the rational dropout framing while maintaining deterrent capability
 *   - Strategic Stability Epistemic Community: Secondary beneficiary/victim (institutional/constrained) — benefits from career dependence on rational-choice framing but constrained by that framing's logical implications
 *   - Disarmament Coalition: Organized agent (organized/mobile) — sees constraint as temporary structure with sunset clause; building alternative verification and transparency regimes
 *   - Regional Powers: Moderate agent (moderate/constrained) — experiences mixed coordination (escalation avoidance) and extraction (conventional disadvantage under nuclear threat)
 *   - Military Establishment: Institutional observer (institutional/constrained) — maintains performative nuclear posture despite rational dropout logic rendering use irrational
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.38).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.65).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Impossibility Kernel — Rational Dropout Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '6632ec83-443c-4c4b-bca4-7278d83cdb62').
narrative_ontology:cs_kernel_codification('6632ec83-443c-4c4b-bca4-7278d83cdb62', distributed).
narrative_ontology:cs_authority_grounding('6632ec83-443c-4c4b-bca4-7278d83cdb62', extraction).
narrative_ontology:cs_interpretation_layer_present('6632ec83-443c-4c4b-bca4-7278d83cdb62').
narrative_ontology:cs_reading_relation('6632ec83-443c-4c4b-bca4-7278d83cdb62', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('6632ec83-443c-4c4b-bca4-7278d83cdb62', nuclear_impossibility_kernel__structural_contraction_reading, influences).
narrative_ontology:cs_axiom('6632ec83-443c-4c4b-bca4-7278d83cdb62', foundational, war_rationally_excluded_but_reachable).
narrative_ontology:cs_axiom_status(war_rationally_excluded_but_reachable, holdable).
narrative_ontology:cs_axiom_grounding('6632ec83-443c-4c4b-bca4-7278d83cdb62', war_rationally_excluded_but_reachable, empirically_contingent).
narrative_ontology:cs_axiom('6632ec83-443c-4c4b-bca4-7278d83cdb62', foundational, deterrence_credibility_via_capacity_readiness).
narrative_ontology:cs_axiom_status(deterrence_credibility_via_capacity_readiness, holdable).
narrative_ontology:cs_axiom_grounding('6632ec83-443c-4c4b-bca4-7278d83cdb62', deterrence_credibility_via_capacity_readiness, instrumental).
narrative_ontology:cs_reference_frame('6632ec83-443c-4c4b-bca4-7278d83cdb62', rational_cost_benefit_strategic_restraint).
narrative_ontology:cs_drift_state('6632ec83-443c-4c4b-bca4-7278d83cdb62', contemporary_post_cold_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6632ec83-443c-4c4b-bca4-7278d83cdb62', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_epistemic_community).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, escalation_risk_bearing_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, conventional_war_possibility_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATE (SNARE) — Faces the constraint but cannot exit it through rational choice. War remains structurally possible (the rational dropout reading preserves war in the reachable set), and threat of nuclear escalation is the sole tool of the nuclear-armed aggressor. The trapped agent bears maximum extraction: subordination under the threat of nuclear use, with no rational exit path available. Cannot acquire nuclear weapons (proliferation barriers), cannot credibly deter (asymmetric arsenal), cannot escalate (risks nuclear response).
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWER (TANGLED ROPE) — Has capacity to deter conventional attack through conventional superiority or alliance, but the nuclear threat creates suppression that constrains exit options. The constraint coordinates escalation avoidance (both parties benefit from not crossing the nuclear threshold) but extracts asymmetric cost: the regional power must accept conventional disadvantage to stay below the nuclear threshold. Mixed experience: genuine coordination function + significant asymmetric extraction.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR-ARMED STATE — DETERRENT LOGIC (ROPE) — Experiences the constraint as coordination mechanism: maintaining credible second-strike capability enables both deterrence stability and avoidance of the mutual destruction outcome. The rational dropout reading preserves war as theoretically possible but practically abandoned, which is the essence of successful deterrence. Beneficiary from the extraction of strategic subordination by non-nuclear states, but also genuinely coordinating avoidance of mutual annihilation. Arbitrage exit (can conduct conventional war, can maintain nuclear arsenal) produces low experienced extraction for this agent.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISARMAMENT COALITION (SCAFFOLD) — Organized actors (NPT signatories, CTBT communities, nuclear abolition movements) see the rational dropout constraint as a temporary structural feature with sunset logic: as verification technologies improve, transparency regimes deepen, and proliferation barriers strengthen, the rationality of nuclear deterrence itself becomes optional rather than mandatory. The coalition experiences constrained exit (faces state-level opposition) but sees a structural path forward. Theater is moderate because the disarmament project is partly performative (lacks enforcement mechanisms) but partly functional (verification and transparency norms are building).
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC STABILITY EPISTEMIC COMMUNITY (TANGLED ROPE) — Analysts, strategic planners, and policy intellectuals who maintain the rational dropout framing benefit from it institutionally: their career paths, publication venues, security clearances, and policy influence depend on treating nuclear deterrence as rationally justified by the cost-benefit calculus. Yet they are also constrained by the framing's own logic: the better they do their job (proving nuclear war is irrational), the more the constraint extracts from the institutions that depend on the deterrent threat. Genuine coordination function (preventing miscalculation) overlaid with extractive institutional dynamics (career path dependence, epistemic monoculture, suppression of alternative framings like structural_contraction or credibility_paradox readings).
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the rational dropout reading can appear as an immutable structural fact: once nuclear weapons exist, rational choice theory *necessarily* produces the conclusion that their use is suicidal. This perspective risks naturalizing what is actually a contingent strategic choice grounded in specific institutional frameworks and value hierarchies. The mountain classification is a false-summit candidate — the constraint is called 'rational' but rationality itself depends on whose rationality, what utility function, what time horizon, and what epistemic community defines the problem.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: MILITARY ESTABLISHMENT (PITON) — Day-to-day military planning, nuclear command authority, deterrent posture maintenance, and nuclear readiness procedures are substantially performative at this point in the constraint's lifecycle. The apparatus maintains itself through inertia, institutional investment, and the residual credibility that derives from being ready to do something everyone knows must never be done. Theater ratio is high because the operational constraint (continuous alert, launch procedures, command authority) persists even though the strategic constraint (rational dropout) has rendered actual use catastrophically irrational. The institution sees itself as degraded — maintaining readiness for an eventuality that rationality has foreclosed.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__rational_dropout_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, TR),
    TR >= 0.70.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The rational dropout reading creates asymmetric advantage for nuclear-armed states over non-nuclear ones, but the extraction is constrained by the mutual-destruction logic that defines the constraint. Nuclear-armed states cannot easily coerce non-nuclear states into total subordination (because that risks conflict escalation), and non-nuclear states have some capacity to deter through conventional strength or alliance. The extraction is structural and significant but not maximal. Suppression (0.65): Moderately high. The constraint operates through genuine suppression of alternatives: non-nuclear states cannot acquire nuclear weapons (NPT + proliferation barriers), cannot credibly threaten nuclear escalation (inferior arsenal), and cannot publicly reject the rational dropout logic without appearing irrational to international community. The suppression has deepened as the regime has matured. Theater (0.58): Moderate-high. The operational nuclear posture (continuous alert, command procedures, readiness drills) is substantially performative: the rationality of use has been proven to be near-zero, yet the apparatus persists through institutional inertia and the residual belief that credibility requires demonstrating capacity for something that must never occur. The theater has increased as the logical case against use has strengthened.
 *
 * PERSPECTIVAL GAP:
 *   The rational dropout reading produces maximum perspectival divergence. Non-nuclear states (trapped/powerless) see pure extraction (snare): they are dominated by a threat that exploits the rational dropout logic to suppress their alternatives. Nuclear-armed states (arbitrage/institutional) see coordination (rope): maintaining deterrent credibility requires being ready to do something everyone recognizes must never be done. The strategic stability community (constrained/institutional) sees tangled rope: genuine coordination function (preventing miscalculation) overlaid with extractive institutional dynamics (career path dependence on the rational-choice framing). The disarmament coalition (mobile/organized) sees a temporary structure with sunset (scaffold): transparency and verification technologies are building an exit path. The analytical observer risks the mountain classification (naturalizing the rationality as inevitable), but structural data reveals this as a false summit — the constraint depends on specific institutional choices and epistemic commitments that could be different.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the extraction flow. Non-nuclear states are pure targets (high d, high f(d), high experienced chi): they cannot exit, they bear the full cost of subordination, and they benefit minimally from deterrence stability. Nuclear-armed states are beneficiaries with arbitrage options (low d, negative f(d)): they can conduct conventional warfare, maintain arsenals, and extract strategic advantage. The strategic stability community occupies an intermediate position (moderate d): they benefit institutionally from the rational-choice framing but are constrained by that framing's logic — the better they do their job proving nuclear war is irrational, the more the constraint suppresses the use-threat they depend on intellectually.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_dropout_vs_structural_contraction,
    'Is nuclear war irrational because costs exceed benefits (rational dropout reading) or because the outcome is physically impossible regardless of cost-benefit calculation (structural contraction reading)?',
    'Comparison of strategic doctrines: does deterrence doctrine depend on maintaining the possibility of use-as-last-resort (rational dropout requires this — war remains reachable), or does it deny reachability entirely (structural contraction)? Review strategic briefing documents, war plans, and policy statements for whether they preserve war as a contingency vs. foreclose it as logically impossible.',
    'If rational dropout: actors maintain credible threat capacity through belief in last-resort rationality; deterrence depends on this belief structure. If structural contraction: deterrence is based on mutual recognition of absolute impossibility; threat lacks credibility but irrelevance provides stability. Different reading leads to different vulnerability profiles and different policies on nuclear-weapon-free zones, disarmament timelines, and command authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_dropout_vs_structural_contraction, conceptual, 'Rational irrationality vs. structural impossibility distinction').

omega_variable(
    credibility_paradox_interaction,
    'Can the rational dropout reading maintain credible deterrence while simultaneously proving that use is irrational? Does the proof of irrationality undermine the threat''s credibility (credibility paradox reading)?',
    'Historical analysis of deterrence stability during periods of maximum strategic parity (1960s-1980s, 2010s-present): did public acknowledgment that nuclear war is irrational increase or decrease the perceived credibility of the deterrent threat? Did states that emphasize rational-choice justifications for their arsenals face higher or lower escalation risks?',
    'If rational dropout is credibility-maintaining: the framing is self-supporting — explicitly proving irrationality prevents the irrational attempt. If paradox reading is correct: the proof of irrationality makes the threat unbelievable, requiring states to re-establish credibility through deliberate irrationality or ambiguity (game-theoretic signaling). Changes the interpretation of nuclear doctrines emphasizing ''calculated ambiguity'' vs. ''transparent rationality.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credibility_paradox_interaction, empirical, 'Whether rational-choice framing maintains or undermines deterrent credibility').

omega_variable(
    reading_specificity_ambiguity,
    'This reading assumes a specific model of rationality: individual actors compare costs and benefits at a specific decision point and choose war if and only if benefits exceed costs. But which rationality? Whose decision point? What time horizon for costs and benefits?',
    'Decomposition of ''rational choice'' into constituent axioms: neoclassical utility maximization vs. institutional decision-making vs. evolutionary stability vs. behavioral heuristics. Each produces different cost-benefit comparisons and different conclusions about whether war is ''rational.'' The reading depends on a hidden commitment to a specific rationality model.',
    'If neoclassical expected-utility dominates: the rational dropout reading works — war is unambiguously irrational. If institutional inertia dominates: organizations may pursue war for reasons orthogonal to cost-benefit (organizational reputation, sunk investment in capability, interagency dynamics). If evolutionary stability dominates: the constraint is not about rationality but about fitness-landscape topology. Different rationality models lead to different predictions about when the constraint might fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_ambiguity, conceptual, 'Hidden dependency on specific rationality model').

omega_variable(
    epistemic_community_cage,
    'The strategic stability epistemic community benefits institutionally from the rational dropout framing (career paths, security clearances, policy relevance). Does this create a suppression mechanism that prevents consideration of alternative readings (credibility paradox, structural contraction) even when evidence supports them?',
    'Discourse analysis of published strategic doctrine, academic journals, and policy think tanks: frequency of publication on alternative readings, citation patterns, career outcomes for researchers proposing non-standard readings, funding flows to institutions defending vs. challenging the rational dropout framing.',
    'If institutional capture is significant: the rational dropout reading is partially enforced by career suppression rather than by superior logic. This would mean the constraint''s core epistemic justification (rationality of the dropout) is itself suppressed by institutional interests in maintaining that justification. Suggests fragility — if the epistemic community''s credibility erodes, the entire rational dropout justification becomes contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_community_cage, empirical, 'Whether epistemic community institutional interests suppress alternative readings').

omega_variable(
    m_set_reachability_claim,
    'This reading asserts that ''war remains in the reachable set but dropped from active consideration'' — that war is M-set-accessible but D-set-excluded. But what evidence demonstrates reachability rather than true structural impossibility?',
    'Examination of what would be required to make war rational under this reading: identify the cost threshold below which war would re-enter active decision-making (i.e., what would have to change about the cost-benefit ratio?). If no plausible cost reduction would make war rational (because mutual annihilation is inevitable), then the reading collapses into structural contraction. If specific cost reductions or benefits would rationally justify war (limited war, contained escalation, regional conflict), then reachability is real.',
    'If truly reachable: states have rational contingency paths to war that depend on cost perception; deterrence relies on maintaining high perceived costs. If unreachable but claimed reachable: the reading is aspirational — it wants reachability to be true (to preserve deterrent credibility) even though the structure may have foreclosed it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_reachability_claim, conceptual, 'Whether war actually remains structurally reachable or is merely claimed reachable for strategic reasons').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuke_dropout_theater_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nuke_dropout_theater_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(nuke_dropout_theater_t40, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(nuke_dropout_extract_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nuke_dropout_extract_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(nuke_dropout_extract_t40, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(nuke_dropout_suppress_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nuke_dropout_suppress_t20, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(nuke_dropout_suppress_t40, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% The nuclear impossibility kernel decomposes into three structurally distinct readings with different ε values and different classification profiles. This story (rational_dropout_reading) models the constraint as rational-choice-based exclusion of war from active consideration (ε=0.38, tangled_rope from dominant perspectives). Sibling readings have different ε values and different structural mechanisms: credibility_paradox emphasizes the logical contradiction in deterrent threats (likely higher ε, snare-dominant); structural_contraction emphasizes physical inevitability of mutual annihilation (likely lower ε, mountain candidate). All three are linked via affects_constraints to enable network analysis of how readings contaminate each other and what the epistemic stakes are in choosing among them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
