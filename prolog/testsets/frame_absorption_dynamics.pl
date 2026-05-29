% ============================================================================
% CONSTRAINT STORY: frame_absorption_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frame_absorption_dynamics, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: frame_absorption_dynamics
 *   human_readable: Frame Absorption Dynamics in Explanatory Models
 *   domain: epistemology/cognitive_science/social_psychology
 *
 * SUMMARY:
 *   Frame absorption dynamics describe the process by which explanatory
 *   frames in social science and policy research absorb statistical variation
 *   from hidden causal variables without making those variables visible to
 *   inquiry. The canonical example: gender as an explanatory variable in
 *   language research absorbs variation that actually comes from register
 *   (formal vs. informal speech contexts). When researchers remove gender
 *   from the model, register emerges as the true causal mechanism — but this
 *   removal rarely occurs because gender is statistically sufficient. The
 *   constraint coordinates legitimate research activity (shared explanatory
 *   categories, cumulative knowledge building, policy-relevant
 *   simplification) while simultaneously extracting from epistemic depth
 *   (hidden variables remain invisible, preventing accurate causal
 *   understanding and targeted intervention). This is a structural Tangled
 *   Rope: the coordination function is genuine (frames enable research to
 *   proceed), but the extraction is also genuine (absorbed variables
 *   systematically evade inquiry). The constraint's extractiveness has
 *   increased over the interval (0.42 → 0.58) as methodological
 *   standardization has made frame-challenging research more difficult, and
 *   theater ratio has increased (0.35 → 0.48) as statistical sophistication
 *   has grown without corresponding growth in causal inquiry depth. The
 *   upstream constraint (explanatory_closure_mechanism) establishes that
 *   explanatory frames naturally resist revision once they achieve
 *   statistical sufficiency — frame absorption is the specific mechanism by
 *   which this resistance operates in the presence of hidden variables.
 *
 * KEY AGENTS:
 *   - Inquiry Depth (Epistemic Commons): Primary victim (powerless/trapped) — abstract collective good with no advocate; bears full cost of degraded causal understanding
 *   - Marginalized Subgroups: Primary victim (powerless/constrained) — groups whose causal mechanisms are hidden behind absorbed variables; bear material costs of invisible mechanisms
 *   - Methodological Rigor Advocates: Mixed position (moderate/constrained) — researchers who recognize frame absorption but face career and resource barriers to challenging sufficient frames
 *   - Institutional Researchers: Primary beneficiary (institutional/arbitrage) — established researchers using conventional frames; benefit from simplified models and protection of existing work
 *   - Policy Makers: Primary beneficiary (institutional/arbitrage) — benefit from frame absorption's simplification function; can switch frames when politically expedient
 *   - Methodological Reform Movements: Organized agents (organized/mobile) — open science, replication reformers, causal inference methodologists building alternative practices
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the structural hybridity: genuine coordination function coexisting with genuine extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frame_absorption_dynamics, 0.58).
domain_priors:suppression_score(frame_absorption_dynamics, 0.62).
domain_priors:theater_ratio(frame_absorption_dynamics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frame_absorption_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(frame_absorption_dynamics, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(frame_absorption_dynamics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frame_absorption_dynamics, tangled_rope).
narrative_ontology:human_readable(frame_absorption_dynamics, "Frame Absorption Dynamics in Explanatory Models").
narrative_ontology:topic_domain(frame_absorption_dynamics, "epistemology/cognitive_science/social_psychology").

domain_priors:requires_active_enforcement(frame_absorption_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frame_absorption_dynamics, frame_users).
narrative_ontology:constraint_beneficiary(frame_absorption_dynamics, institutional_researchers).
narrative_ontology:constraint_beneficiary(frame_absorption_dynamics, policy_makers).
narrative_ontology:constraint_victim(frame_absorption_dynamics, inquiry_depth).
narrative_ontology:constraint_victim(frame_absorption_dynamics, marginalized_subgroups).
narrative_ontology:constraint_victim(frame_absorption_dynamics, methodological_rigor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INQUIRY DEPTH (SNARE) — The epistemic commons has no advocate and cannot exit the frame absorption trap. Suffers maximum extraction: explanatory frames that absorb variation without revealing hidden variables prevent deeper inquiry from occurring. The collective capacity for accurate causal understanding is systematically degraded, with no mechanism for self-correction when frames are sufficient-but-wrong.
constraint_indexing:constraint_classification(frame_absorption_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED SUBGROUPS (SNARE) — Groups whose causal mechanisms are hidden behind absorbed variables bear direct material costs. When gender absorbs register, or race absorbs class, the specific mechanisms affecting subgroups remain invisible to policy and intervention. High exit costs: challenging the dominant frame requires resources, credibility, and institutional access that marginalized groups typically lack.
constraint_indexing:constraint_classification(frame_absorption_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: METHODOLOGICAL RIGOR ADVOCATES (TANGLED ROPE) — Researchers who recognize frame absorption face mixed incentives. The constraint coordinates legitimate work (statistical modeling, variable selection) but also extracts: challenging sufficient frames is career-risky, resource-intensive, and often unrewarded. Benefits from the coordination function (shared statistical methods) while bearing costs of suppressed inquiry.
constraint_indexing:constraint_classification(frame_absorption_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL RESEARCHERS (ROPE) — Established researchers using conventional frames experience this as pure coordination. Sufficient frames enable publication, funding, and policy influence. Can exit to alternative frames when convenient (arbitrage across explanatory models). Net beneficiaries: the frame absorption mechanism protects their existing work from costly reanalysis.
constraint_indexing:constraint_classification(frame_absorption_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY MAKERS (ROPE) — Policy actors benefit from frame absorption's simplification function. Sufficient frames enable actionable interventions without requiring deep causal understanding. Can switch frames when politically expedient. Experience the constraint as coordination: frames organize evidence into usable categories for policy design.
constraint_indexing:constraint_classification(frame_absorption_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: METHODOLOGICAL REFORM MOVEMENTS (TANGLED ROPE) — Organized coalitions (open science, replication crisis reformers, causal inference methodologists) see both coordination and extraction. The constraint coordinates legitimate statistical practice but also suppresses inquiry into hidden variables. Mobile exit: can build alternative methodological communities, but face institutional resistance.
constraint_indexing:constraint_classification(frame_absorption_dynamics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, frame absorption is a hybrid mechanism. It genuinely coordinates inquiry (shared explanatory categories enable cumulative research) while simultaneously extracting from epistemic depth (absorbed variables remain invisible, preventing causal understanding). The constraint is not reducible to either pure coordination or pure extraction — it is structurally both.
constraint_indexing:constraint_classification(frame_absorption_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frame_absorption_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frame_absorption_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frame_absorption_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frame_absorption_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(frame_absorption_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Frame absorption extracts from epistemic depth by preventing inquiry into hidden variables, and this extraction has material consequences for marginalized subgroups whose causal mechanisms remain invisible. The value reflects that much research proceeds without recognizing absorbed variables exist, and challenging sufficient frames is career-risky and resource-intensive. However, extraction is not maximal — some researchers do conduct control-condition experiments, and methodological reform movements are building alternative practices. Suppression (0.62): Moderate-high. Significant barriers to frame-challenging research include: statistical sufficiency creates methodological inertia (why remove a variable that 'works'?), institutional review favors frame-confirming research, career incentives reward using conventional frames, resource requirements for control-condition designs are substantial, and publication bias against null results (when frame removal reveals no hidden variable). But suppression is not total — control-condition methodology exists, some journals reward methodological innovation, and reform movements have institutional footholds. Theater ratio (0.48): Moderate. Statistical sophistication (complex models, large datasets, advanced techniques) has grown substantially, but this sophistication often operates within absorbed frames rather than challenging them. The theater is the performance of rigor without the corresponding depth of causal inquiry. However, theater is not dominant — much statistical work is genuinely functional, and the constraint coordinates real research progress.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a clean three-way perspectival split. Beneficiaries (institutional researchers, policy makers) experience frame absorption as pure coordination — sufficient frames enable their work to proceed efficiently, and they can exit to alternative frames when convenient. Victims (inquiry depth, marginalized subgroups) experience frame absorption as pure extraction — hidden variables remain invisible, preventing accurate causal understanding and targeted intervention, with no exit option. The analytical observer and organized reform movements see the structural hybridity: frame absorption genuinely coordinates research (shared categories, cumulative knowledge) while simultaneously extracting from epistemic depth (absorbed variables evade inquiry). The gap reveals that 'sufficiency' is not a neutral statistical property — it is a structural position. What appears as efficient coordination from the beneficiary position appears as systematic epistemic degradation from the victim position. The Tangled Rope classification at the analytical level captures this irreducible duality: the constraint is not reducible to either coordination or extraction alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Inquiry depth (powerless/trapped victim) experiences maximum extraction — d ≈ 0.95, f(d) ≈ 1.42. Marginalized subgroups (powerless/constrained victim) experience high extraction — d ≈ 0.85, f(d) ≈ 1.15. Methodological rigor advocates (moderate/constrained, both victim and beneficiary) experience moderate extraction — d ≈ 0.55, f(d) ≈ 0.75. Institutional researchers (institutional/arbitrage beneficiary) experience low or negative extraction — d ≈ 0.05, f(d) ≈ -0.12. Policy makers (institutional/arbitrage beneficiary) similarly experience low extraction — d ≈ 0.05, f(d) ≈ -0.12. Methodological reform movements (organized/mobile, mixed position) experience moderate extraction — d ≈ 0.40, f(d) ≈ 0.40. The analytical observer (analytical/analytical) uses the canonical analytical d ≈ 0.72, f(d) ≈ 1.15. The perspectival gap is substantial: beneficiaries see coordination (Rope), victims see extraction (Snare), and the analytical position sees the structural hybridity (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that frame absorption is structurally both coordination and extraction, not one masquerading as the other. The coordination function is genuine: explanatory frames enable research to proceed, organize evidence into policy-relevant categories, and support cumulative knowledge building. The extraction function is also genuine: absorbed variables remain invisible to inquiry, preventing accurate causal understanding and systematically disadvantaging groups whose mechanisms are hidden. This is not a case of 'extraction disguised as coordination' (which would be pure Snare) or 'coordination with incidental costs' (which would be Rope with externalities). It is a structural hybrid where the same mechanism performs both functions simultaneously. The Tangled Rope classification is not a compromise or a failure to choose — it is the accurate description of a constraint that genuinely coordinates while genuinely extracting. The mandatrophy is resolved by recognizing that some constraints are irreducibly hybrid, and the framework's job is to measure both functions accurately rather than forcing a binary choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_threshold,
    'What statistical threshold distinguishes a legitimately sufficient explanatory frame from one that absorbs hidden variables extractively?',
    'Systematic control-condition experiments across domains: remove the sufficient frame and measure how often previously invisible variables emerge with comparable or superior explanatory power. Track the distribution of effect sizes for absorbed vs. revealed variables.',
    'If threshold is low (e.g., R² > 0.3): many extractive frames are misclassified as legitimate. If threshold is high (e.g., R² > 0.7): legitimate simplifications are misclassified as extractive absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_threshold, empirical, 'Statistical threshold for distinguishing legitimate sufficiency from extractive absorption').

omega_variable(
    frame_removal_feasibility,
    'Is removing a sufficient frame to reveal hidden variables methodologically feasible in most research contexts, or does it require prohibitive resources?',
    'Cost-benefit analysis of control-condition designs across disciplines. Survey of researchers who attempted frame-removal experiments: success rates, resource requirements, publication outcomes, career impacts.',
    'If feasible: frame absorption is a coordination problem with available solutions (Scaffold from more perspectives). If prohibitive: frame absorption is an extraction mechanism with high suppression (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frame_removal_feasibility, empirical, 'Feasibility of frame-removal methodology').

omega_variable(
    institutional_resistance_mechanism,
    'Does institutional resistance to frame-challenging research stem from legitimate methodological conservatism or from protection of existing research programs?',
    'Comparative analysis of review outcomes for frame-challenging vs. frame-confirming research with equivalent methodological rigor. Track career trajectories of researchers who challenge vs. confirm dominant frames.',
    'If legitimate conservatism: higher suppression is justified (Tangled Rope with higher coordination value). If protective: suppression is extractive (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resistance_mechanism, empirical, 'Source of institutional resistance to frame-challenging research').

omega_variable(
    absorbed_variable_distribution,
    'Are absorbed variables distributed randomly across research domains, or do they systematically cluster around marginalized populations?',
    'Meta-analysis of control-condition experiments: correlate the presence of absorbed variables with the demographic characteristics of affected populations. Test whether frame absorption is more common when hidden variables affect low-power groups.',
    'If random: frame absorption is a general epistemic problem (coordination failure). If clustered: frame absorption is a structural extraction mechanism targeting specific populations (Snare with identifiable victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absorbed_variable_distribution, empirical, 'Distribution pattern of absorbed variables across populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frame_absorption_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frame_abs_tr_t0, frame_absorption_dynamics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(frame_abs_tr_t3, frame_absorption_dynamics, theater_ratio, 3, 0.4).
narrative_ontology:measurement(frame_abs_tr_t6, frame_absorption_dynamics, theater_ratio, 6, 0.44).
narrative_ontology:measurement(frame_abs_tr_t10, frame_absorption_dynamics, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(frame_abs_be_t0, frame_absorption_dynamics, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(frame_abs_be_t3, frame_absorption_dynamics, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(frame_abs_be_t6, frame_absorption_dynamics, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(frame_abs_be_t10, frame_absorption_dynamics, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frame_absorption_dynamics, information_standard).

% DUAL FORMULATION NOTE:
% Frame absorption dynamics is downstream of explanatory_closure_mechanism (the general resistance of sufficient frames to revision). The upstream constraint establishes the closure mechanism; this constraint specifies how closure operates in the presence of hidden variables. The two constraints have different extractiveness values: explanatory_closure_mechanism is a Mountain (ε ≈ 0.08, reflecting the inherent difficulty of revising sufficient explanations), while frame_absorption_dynamics is a Tangled Rope (ε = 0.58, reflecting the career and institutional barriers to frame-challenging research layered on top of the inherent difficulty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
