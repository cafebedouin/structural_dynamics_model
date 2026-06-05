% ============================================================================
% CONSTRAINT STORY: chip_constraint_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chip_constraint_bottleneck, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: chip_constraint_bottleneck
 *   human_readable: U.S. Chip Export Restrictions as Computing Power Ceiling
 *   domain: technology_governance/surveillance_studies/export_control
 *
 * SUMMARY:
 *   U.S. export controls on advanced semiconductor manufacturing equipment
 *   and high-performance GPUs (A100, H100, and successors) create a computing
 *   power ceiling for Chinese AI development. This constraint operates at the
 *   intersection of technology governance, national security strategy, and
 *   global AI research infrastructure. The 2022-2024 restrictions target both
 *   manufacturing capability (EUV lithography, advanced packaging) and
 *   end-use products (datacenter GPUs above specified performance
 *   thresholds). Leaked documents from Chinese AI research institutions show
 *   deployment scope vs ambition gaps: predictive surveillance models are
 *   limited to older architectures (V100-equivalent or domestic alternatives
 *   with 2-3 generation performance lag), and model sophistication metrics
 *   show a widening gap relative to U.S. frontier models. The constraint
 *   exhibits tangled rope structure from the analytical perspective: genuine
 *   coordination function (preventing AI-enabled authoritarian capacity
 *   concentration) coexists with asymmetric extraction (technological
 *   decoupling costs, research balkanization, strategic advantage
 *   concentration). The theater ratio (0.35) reflects moderate performative
 *   content: export control enforcement includes substantial verification
 *   theater (end-use monitoring, supply chain audits) but also real
 *   interdiction capacity. The constraint is downstream of
 *   predictive_surveillance_extractiveness — chip restrictions limit the
 *   sophistication of surveillance models that would otherwise extract more
 *   from monitored populations.
 *
 * KEY AGENTS:
 *   - Chinese AI Research Community: Primary victim (powerless/trapped) — career trajectories depend on model sophistication but hardware ceiling prevents competitive research; no individual exit path from national infrastructure constraints
 *   - Chinese Surveillance State Apparatus: Institutional victim with coordination benefit (institutional/constrained) — predictive model ceiling limits operational capability but export controls force strategic autonomy and indigenous innovation
 *   - U.S. National Security Apparatus: Primary beneficiary (institutional/arbitrage) — strategic advantage from AI capability asymmetry; intelligence collection benefits from adversary model limitations
 *   - U.S. Semiconductor Industry: Powerful beneficiary with sunset logic (powerful/mobile) — short-term market protection and R&D incentives; expects eventual normalization
 *   - Global AI Research Collaboration Networks: Organized agents experiencing mixed effects (organized/mobile) — balkanization of research ecosystems vs incentive for distributed architectures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (authoritarian capacity prevention) and real extraction (duplicated infrastructure, slowed safety research)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chip_constraint_bottleneck, 0.38).
domain_priors:suppression_score(chip_constraint_bottleneck, 0.42).
domain_priors:theater_ratio(chip_constraint_bottleneck, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chip_constraint_bottleneck, extractiveness, 0.38).
narrative_ontology:constraint_metric(chip_constraint_bottleneck, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(chip_constraint_bottleneck, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chip_constraint_bottleneck, tangled_rope).
narrative_ontology:human_readable(chip_constraint_bottleneck, "U.S. Chip Export Restrictions as Computing Power Ceiling").
narrative_ontology:topic_domain(chip_constraint_bottleneck, "technology_governance/surveillance_studies/export_control").

domain_priors:requires_active_enforcement(chip_constraint_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chip_constraint_bottleneck, us_semiconductor_industry).
narrative_ontology:constraint_beneficiary(chip_constraint_bottleneck, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(chip_constraint_bottleneck, domestic_ai_research_institutions).
narrative_ontology:constraint_victim(chip_constraint_bottleneck, chinese_ai_research_community).
narrative_ontology:constraint_victim(chip_constraint_bottleneck, chinese_surveillance_state_capacity).
narrative_ontology:constraint_victim(chip_constraint_bottleneck, global_ai_research_collaboration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE AI RESEARCH COMMUNITY (SNARE) — Trapped by national infrastructure constraints and export controls. Cannot access cutting-edge GPU architectures (A100/H100 restricted). Career trajectories depend on model sophistication but hardware ceiling prevents competitive research. Maximum experienced extraction — structural barriers with no individual exit path.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHINESE SURVEILLANCE STATE (TANGLED ROPE) — Constrained by chip availability but also benefits from the coordination function: export controls force development of domestic semiconductor capacity and alternative architectures. Significant extraction (predictive model sophistication ceiling limits operational capability) but also genuine coordination benefit (strategic autonomy, indigenous innovation). Mixed experience.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. NATIONAL SECURITY APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as coordination: export controls solve the collective action problem of preventing adversary AI capability advancement. Net beneficiary — extraction runs toward this agent (strategic advantage, intelligence asymmetry) not away from them.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL AI RESEARCH COLLABORATION (TANGLED ROPE) — Organized agents (academic consortia, open-source communities, international research partnerships) experience both coordination (export controls create incentive for distributed compute architectures, federated learning) and extraction (balkanization of research ecosystems, reduced cross-border collaboration, duplicated effort). Mobile exit options (can relocate research, use cloud compute in neutral jurisdictions) but constrained by national security restrictions.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: U.S. SEMICONDUCTOR INDUSTRY (SCAFFOLD) — Powerful beneficiary with mobile exit options (can lobby for policy adjustment, develop alternative markets). Sees the constraint as temporary coordination: export controls create short-term market protection and R&D incentives, but the industry expects eventual normalization as domestic Chinese capacity matures or geopolitical tensions resolve. Low effective extraction because the industry has agency and sees the restriction as a transitional strategic tool rather than permanent structure.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the chip bottleneck exhibits both genuine coordination (preventing AI-enabled authoritarian capacity concentration) and asymmetric extraction (technological decoupling creates duplicated infrastructure costs, slows global AI safety research, and concentrates compute power in U.S.-aligned jurisdictions). The constraint is not a natural law — it is a contingent policy choice with real coordination benefits and real extraction costs.
constraint_indexing:constraint_classification(chip_constraint_bottleneck, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chip_constraint_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chip_constraint_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chip_constraint_bottleneck, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(chip_constraint_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint creates real asymmetry — Chinese AI researchers face a 2-3 generation performance lag, limiting model sophistication and career competitiveness. But extraction is not maximal: workarounds exist (cloud compute arbitrage, smuggling networks, architectural efficiency improvements), and the coordination function (preventing authoritarian AI capacity concentration) is genuine. The value reflects that career asymmetry and research balkanization are real costs, but the constraint is not pure rent-seeking. Suppression (0.42): Moderate. Significant barriers include export licensing requirements, end-use monitoring, supply chain interdiction, and criminal penalties for violations. But suppression is not total — alternative suppliers exist (though with performance penalties), domestic manufacturing is advancing (slowly), and cloud compute in neutral jurisdictions provides partial workarounds. The rising trajectory (0.35 → 0.42) reflects enforcement intensification as the U.S. closes loopholes and expands restricted entity lists. Theater ratio (0.35): Moderate-low. Export control enforcement includes substantial verification theater (compliance audits, end-use certifications, supply chain documentation) but also real interdiction capacity (customs seizures, entity list enforcement, third-country pressure). The theater has increased as enforcement bureaucracy has expanded, but the core mechanism (denying physical access to restricted hardware) remains functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same export control policy appears as snare, tangled rope, rope, or scaffold depending on the observer's structural position. The Chinese AI research community sees a snare — trapped by infrastructure constraints with no individual exit, bearing maximum career and research costs. The Chinese surveillance state sees tangled rope — constrained by chip availability but also benefiting from forced indigenous innovation. The U.S. national security apparatus sees rope — solving the coordination problem of preventing adversary AI capability advancement. The U.S. semiconductor industry sees scaffold — temporary market protection with expected sunset as geopolitical tensions resolve or domestic Chinese capacity matures. The analytical observer sees tangled rope — genuine coordination (authoritarian capacity prevention) coexisting with real extraction (research balkanization, duplicated infrastructure, strategic advantage concentration). The perspectival gap reveals that 'is this policy legitimate?' depends on which structural position you measure from — the coordination function is real for the beneficiary, the extraction is real for the victim, and both are real from the analytical perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The Chinese AI research community is the primary victim with trapped exit options — they bear maximum extraction because they cannot individually escape national infrastructure constraints. The Chinese surveillance state is an institutional victim but with constrained rather than trapped exit (can develop domestic alternatives, though at significant cost) — this produces lower effective extraction than the research community experiences. The U.S. national security apparatus is the primary beneficiary with arbitrage exit options — they experience the constraint as coordination (strategic advantage) with minimal cost. The U.S. semiconductor industry is a powerful beneficiary with mobile exit options and sunset logic — they see the constraint as temporary strategic protection. Global research collaboration networks are organized agents with mobile exit options — they experience mixed coordination (incentive for distributed architectures) and extraction (balkanization costs). The analytical observer uses the analytical context and sees the constraint as tangled rope — genuine coordination function coexisting with real asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that export controls are neither pure coordination (rope) nor pure extraction (snare) but a hybrid structure (tangled rope from analytical perspective) whose classification varies by observer position. The U.S. national security framing ('preventing authoritarian AI') emphasizes the coordination function and sees rope. The Chinese research community framing ('technological containment') emphasizes the extraction and sees snare. The analytical perspective sees both: the coordination function is genuine (chip restrictions do limit surveillance model sophistication, as evidenced by deployment scope vs ambition gaps in leaked documents), and the extraction is genuine (career asymmetry, research balkanization, strategic advantage concentration). The mandatrophy is not 'which framing is correct?' but 'which structural position are you measuring from?' The presheaf over observation sites captures the full structure: coordination for the beneficiary, extraction for the victim, hybrid for the analytical observer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_capacity_timeline,
    'How quickly can Chinese domestic semiconductor manufacturing reach parity with restricted U.S. architectures?',
    'Tracking SMIC and other Chinese foundries'' process node advancement; independent verification of domestic GPU performance benchmarks; analysis of indigenous AI model training efficiency',
    'If parity achieved within 5 years: export controls become pure extraction (coordination function fails, only asymmetry remains). If parity requires 10+ years: coordination function is real and substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_capacity_timeline, empirical, 'Timeline for Chinese domestic semiconductor parity').

omega_variable(
    smuggling_and_workaround_effectiveness,
    'Do smuggling networks, cloud compute arbitrage, and architectural workarounds effectively bypass the chip ceiling?',
    'Analysis of leaked procurement documents; model sophistication vs official hardware availability discrepancies; detection of advanced models trained on restricted hardware',
    'If workarounds are effective: suppression is lower than measured, constraint is weaker rope. If workarounds are ineffective: suppression is accurate, constraint is stronger snare from victim perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(smuggling_and_workaround_effectiveness, empirical, 'Effectiveness of chip restriction workarounds').

omega_variable(
    dual_use_boundary_stability,
    'Can export controls distinguish AI research from surveillance applications, or does the dual-use nature make all restrictions inherently over-broad?',
    'Analysis of restricted vs permitted use cases; false positive rate (legitimate research blocked); false negative rate (surveillance applications using permitted chips)',
    'If boundary is stable: coordination function is precise, extraction is minimized. If boundary is unstable: over-broad restrictions increase extraction on non-surveillance research.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_boundary_stability, conceptual, 'Stability of dual-use application boundaries').

omega_variable(
    authoritarian_capacity_counterfactual,
    'Would unrestricted chip access have enabled significantly more effective authoritarian surveillance, or are non-chip factors (data access, legal frameworks, organizational capacity) the binding constraints?',
    'Comparative analysis of surveillance effectiveness across jurisdictions with different compute access; identification of non-chip bottlenecks in surveillance deployment; counterfactual modeling of surveillance capacity with unrestricted hardware',
    'If chips are the binding constraint: coordination function is real and substantial. If non-chip factors dominate: export controls are mostly extraction (strategic posturing) with minimal coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authoritarian_capacity_counterfactual, empirical, 'Whether chip access is the binding constraint on authoritarian surveillance capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chip_constraint_bottleneck, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chip_bottleneck_theater_t0, chip_constraint_bottleneck, theater_ratio, 0, 0.25).
narrative_ontology:measurement(chip_bottleneck_theater_t3, chip_constraint_bottleneck, theater_ratio, 3, 0.3).
narrative_ontology:measurement(chip_bottleneck_theater_t6, chip_constraint_bottleneck, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(chip_bottleneck_extract_t0, chip_constraint_bottleneck, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(chip_bottleneck_extract_t3, chip_constraint_bottleneck, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(chip_bottleneck_extract_t6, chip_constraint_bottleneck, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(chip_bottleneck_suppress_t0, chip_constraint_bottleneck, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(chip_bottleneck_suppress_t3, chip_constraint_bottleneck, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(chip_bottleneck_suppress_t6, chip_constraint_bottleneck, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chip_constraint_bottleneck, enforcement_mechanism).
narrative_ontology:affects_constraint(chip_constraint_bottleneck, predictive_surveillance_extractiveness).

% DUAL FORMULATION NOTE:
% The chip bottleneck is upstream of predictive_surveillance_extractiveness in the causal chain but downstream in the affects_constraints network because the surveillance constraint's extractiveness is the normative motivation for the chip restrictions. The chip bottleneck limits the sophistication of surveillance models that would otherwise extract more from monitored populations. These are structurally distinct constraints: predictive_surveillance_extractiveness has its own epsilon reflecting the career/privacy extraction of surveillance deployment; chip_constraint_bottleneck has its own epsilon reflecting the research asymmetry and strategic advantage concentration of export controls.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
