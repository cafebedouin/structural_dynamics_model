% ============================================================================
% CONSTRAINT STORY: ai_capex_productivity_puzzle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_capex_productivity_puzzle, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_capex_productivity_puzzle
 *   human_readable: AI Capital Expenditure and Productivity Paradox
 *   domain: economic_policy/technology_investment
 *
 * SUMMARY:
 *   The AI Capital Expenditure Productivity Puzzle refers to a structural
 *   constraint in the macroeconomic relationship between frontier AI
 *   development capex and measured productivity returns. Since 2023,
 *   enterprises and governments have committed trillions of dollars to AI
 *   infrastructure, yet measured labor productivity growth remains flat or
 *   declining in many sectors, and total factor productivity shows no
 *   acceleration. This creates a stark contradiction: either (1) the capex is
 *   producing unmeasured productivity gains that GDP statistics miss, (2) the
 *   capex is extractive rent-seeking disguised as productive investment, or
 *   (3) productivity will materialize after a delayed adoption lag. The
 *   constraint exhibits all six DR types depending on structural position:
 *   hardware manufacturers and frontier labs experience it as pure
 *   coordination (Rope); enterprises experience it as mixed
 *   coordination-extraction (Tangled Rope); productivity measurement systems
 *   experience it as pure extraction (Snare); open-source communities
 *   experience it as a temporary problem with a sunset (Scaffold); the
 *   mainstream productivity discourse experiences it as degraded theater
 *   (Piton); and the civilizational view risks naturalizing it as an
 *   immutable technology adoption pattern (Mountain). The theater ratio
 *   (0.65) reflects that the 'productivity revolution' narrative relies
 *   heavily on aspirational framing (future gains, potential unlocked,
 *   paradigm shift coming) rather than current measured returns.
 *
 * KEY AGENTS:
 *   - Hardware Manufacturers: Primary beneficiary (institutional/arbitrage) — capture unprecedented capex demand; experience constraint as pure coordination
 *   - Frontier AI Labs: Primary beneficiary (powerful/mobile) — set research direction; benefit from ecosystem requirement to chase the frontier; have agency to suppress alternative research pathways
 *   - Enterprise Adopters: Secondary victim (moderate/constrained) — face competitive pressure to invest; bear capex burden; receive genuine but unmeasured or misdistributed productivity gains
 *   - Productivity Measurement System: Primary victim (powerless/trapped) — statistical apparatus trapped in blind spot; reports declining TFP despite massive capex; cannot exit or reorganize
 *   - Labor Market: Implicit victim (powerless/constrained) — wage growth stagnation despite AI productivity tools; exposure to substitution effects and measurement-resistant quality losses
 *   - Open-Source AI Community: Organized agents (organized/constrained) — building alternative pathways with lower capex intensity; see the constraint as temporary with sunset mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as immutable technology adoption properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_capex_productivity_puzzle, 0.55).
domain_priors:suppression_score(ai_capex_productivity_puzzle, 0.48).
domain_priors:theater_ratio(ai_capex_productivity_puzzle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_capex_productivity_puzzle, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_capex_productivity_puzzle, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_capex_productivity_puzzle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_capex_productivity_puzzle, tangled_rope).
narrative_ontology:human_readable(ai_capex_productivity_puzzle, "AI Capital Expenditure and Productivity Paradox").
narrative_ontology:topic_domain(ai_capex_productivity_puzzle, "economic_policy/technology_investment").

domain_priors:requires_active_enforcement(ai_capex_productivity_puzzle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_capex_productivity_puzzle, hardware_manufacturers).
narrative_ontology:constraint_beneficiary(ai_capex_productivity_puzzle, cloud_providers).
narrative_ontology:constraint_beneficiary(ai_capex_productivity_puzzle, frontier_ai_labs).
narrative_ontology:constraint_victim(ai_capex_productivity_puzzle, enterprise_productivity_measurement).
narrative_ontology:constraint_victim(ai_capex_productivity_puzzle, macroeconomic_efficiency).
narrative_ontology:constraint_victim(ai_capex_productivity_puzzle, labor_market_wage_growth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRODUCTIVITY MEASUREMENT SYSTEM (SNARE) — Cannot exit the measurement crisis; bears full cost of statistical degradation. The labor productivity index, capital productivity ratio, and total factor productivity (TFP) statistics are trapped in a structural blind spot: trillions in AI capex produce unmeasurable or negative measured productivity returns. The measurement apparatus has no agency and no exit — it can only report what the data shows, which contradicts the investment narrative.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE ADOPTER (TANGLED ROPE) — Constrained by competitive pressure to invest in AI, but also benefits from real productivity gains in specific domains (customer service automation, code generation, content filtering). Genuine coordination function (solving genuine coordination problems) alongside asymmetric extraction (bearing capex burden while competitors free-ride). High suppression because exit is costly — not investing risks market share loss.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HARDWARE MANUFACTURER (ROPE) — Benefits from unprecedented capex demand. Experiences the constraint as pure coordination: communicating GPU scarcity and lead times solves collective action problem of coordinating investment. Net beneficiary — extraction runs reliably toward this agent.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FRONTIER AI LAB (TANGLED ROPE) — Powerful actor with mobile exit options (can secure capital regardless of productivity data, can migrate between jurisdictions). Coordinates genuine research progress (scaling laws, new capabilities) while extracting rent from ecosystem's requirement to chase the AI frontier. Has agency but benefits from suppression of alternative research directions.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PRODUCTIVITY ECONOMICS DISCOURSE (PITON) — The 'AI productivity revolution' narrative persists as institutional theater despite statistical degradation. Academic papers, consulting reports, and policy statements maintain the discourse through aspirational framing (future productivity, potential unlocked, paradigm shift coming) rather than measured current returns. Theater ratio high because the mechanism relies on deferring verification to future observations.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE AI COMMUNITY (SCAFFOLD) — Organized agents (Meta, Hugging Face, community researchers) building alternative AI pathways with lower capex intensity and public benefit orientation. Sees the constraint as temporary — as open-source models mature and democratize access, the frontier-lab bottleneck and associated capex requirements become negotiable. Sunset mechanism: as open models achieve feature parity with closed labs, the capex extraction logic weakens.
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the productivity puzzle reflects an immutable property of general-purpose technology adoption: new technologies always show a productivity paradox (large capex, delayed or unmeasurable returns) before integration matures. This perspective naturalize the constraint as inherent to technological disruption. However, the structural data reveals this as a false summit — the puzzle is not immutable physics but contingent policy choices (capex tax treatment, regulatory asymmetries favoring scale, suppression of alternative research directions).
constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_capex_productivity_puzzle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_capex_productivity_puzzle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_capex_productivity_puzzle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_capex_productivity_puzzle, TR),
    TR >= 0.70.

:- end_tests(ai_capex_productivity_puzzle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The capex-to-productivity conversion shows significant slippage. Trillions committed; measured TFP flat or negative. Some slippage is legitimate (adoption lag, measurement lag, organizational integration time). But the magnitude suggests structural extraction: hardware manufacturers and frontier labs capturing disproportionate value while enterprise productivity gains diffuse unmeasured or concentrate in unmeasurable domains (quality, speed, customer satisfaction that don't translate to GDP). The value has increased from 0.30 in year 0 to 0.55 in year 4, reflecting growing divergence between capex commitments and measured returns. Suppression (0.48): Moderate. Significant barriers to exit the investment treadmill: competitive pressure (enterprises cannot unilaterally de-invest without market share risk), information asymmetry (frontier labs control capability narratives), and policy alignment (governments have staked geopolitical legitimacy on AI leadership). But suppression is not total — some enterprises are experimenting with smaller models, open-source alternatives, and ROI-focused implementations. Theater ratio (0.65): Moderately high. The productivity revolution narrative relies on aspirational framing and capability announcements. Academic papers focus on benchmark performance improvements; consulting reports promise future productivity; policy documents frame AI as necessary for competitiveness. Current measured returns are weak, so the discourse sustains itself through future-tense claims and capability theater rather than empirical productivity data.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals that the constraint's classification depends entirely on structural position. From the beneficiary's perspective (hardware, frontier labs), the constraint solves genuine coordination problems and merits Rope or Tangled Rope. From the victim's perspective (measurement system, labor market), the constraint is extraction with no coordination benefit and merits Snare. From the organized agent's perspective (open-source community), the constraint is temporary and has a sunset mechanism, meriting Scaffold. From the institutional theater perspective (productivity discourse), the constraint is performative and degraded, meriting Piton. The false summit perspective (civilizational/natural law) attempts to transcend the perspectival gap by naturalizing it — 'all general-purpose technology adoption shows this lag' — but the structural data undermines this: the puzzle is not universal (some sectors show strong AI productivity gains immediately) and not immutable (it depends on capex concentration policy choices, research funding allocation, measurement methodology). The true analytical synthesis is Tangled Rope: the constraint genuinely coordinates some productivity gains while structurally extracting from measurement systems and labor markets. The false summit attempt reveals the oracle gap — the civilizational perspective cannot see the contingent policy structures that create the puzzle from within its naturalization frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Hardware manufacturers (GPUs, infrastructure), frontier AI labs (capital allocation, capability primacy). These agents benefit from the capex escalation logic and experience the constraint as enabling rather than constraining. Victims: Productivity measurement systems (statistical degradation), labor markets (wage stagnation despite productivity tools), enterprises bearing capex burden without proportional returns. The victims' experience is structural extraction: forced investment, unmeasured gains, suppressed alternatives. The machinery works as follows: capex escalation is driven by competitive pressure and capability frontier narratives; hardware and lab beneficiaries capture direct rents; enterprise adopters bear costs; productivity gains diffuse unmeasured or misdistribute; labor market experiences substitution pressure without wage compensation; measurement systems report degraded TFP; discourse sustains narrative through theater despite measured weakness. This is pure Tangled Rope mechanics: genuine coordination (capability advancement, infrastructure scaling) alongside asymmetric extraction (capex concentration, measurement degradation, labor suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The AI capex-productivity puzzle should be decomposed into two structurally distinct constraints with different ε values: (1) AI_Frontier_Capability_Coordination (ε≈0.15, Rope) — genuinely solves real research coordination problems, low extraction; (2) AI_Capex_Productivity_Paradox (ε≈0.55, Tangled Rope) — mixes real coordination gains with asymmetric extraction and measurement degradation. The first story captures the real scientific progress and capability advances. The second captures the macroeconomic extraction: capex concentration, labor suppression, measurement system degradation. Together, they explain how one constraint (frontier capability coordination) is genuine and beneficial, while the other (capex productivity paradox) is extractive. Currently, the narrative conflates them — 'AI research requires massive capex, so the capex is justified' — but the ε values differ by a factor of 3.6, signaling distinct mechanisms. The mandatrophy is resolved by decomposing the natural-language concept 'AI productivity' into its structurally precise constraints and recognizing that capability coordination does not require capex concentration. The puzzle exists because institutional choices (funding concentration in frontier labs, hardware-lab coupling, policy alignment with scale narratives) prevent capex from flowing to open-source research, even though open-source scaling laws are identical to closed-lab scaling laws. The mandatrophy resolution: the coordination is real; the extraction is contingent policy, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_measurement_lag,
    'Is the measured productivity decline a genuine statistical artifact of delayed adoption spillover, or a structural sign that capex is extractive rather than coordinative?',
    'Longitudinal cohort analysis: track enterprises by adoption timing and compare TFP trajectories 2, 5, and 10 years post-adoption. If late adopters show accelerated catches, lag is real. If all cohorts show persistent negative TFP delta, the capex is extractive.',
    'If lag: constraint reclassifies toward Rope (productive coordination with delayed measurement). If extractive: constraint reclassifies toward Snare (pure rent extraction disguised as investment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_measurement_lag, empirical, 'Whether measured productivity decline reflects adoption lag or structural extraction').

omega_variable(
    capability_measurement_asymmetry,
    'Are new AI capabilities (reasoning depth, multimodal synthesis, code generation quality) creating unmeasured value that GDP statistics miss?',
    'Construct alternative productivity metrics: task completion speed, error rates, code review time, customer support resolution time, content generation velocity. Compare these against traditional TFP. If alternative metrics show large positive returns while GDP doesn''t, measurement methodology is biased.',
    'If true: extractiveness falls to 0.25-0.35 (measurement artifact, genuine coordination). If false: extractiveness rises to 0.70+ (pure rent extraction, Snare). This omega resolves whether the constraint is a real productivity puzzle or a measurement problem masking extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_measurement_asymmetry, empirical, 'Whether new AI capabilities are creating unmeasured productivity value').

omega_variable(
    capex_concentration_lock_in,
    'Does the capex frontier-model lock-in (enterprises forced to buy latest GPUs, subscribe to frontier APIs) constitute genuine competitive necessity or artificial scarcity created by hardware-lab coordination?',
    'Historical counterfactual: analyze cost trajectories if capex had distributed evenly to open-source infrastructure instead of concentrating in frontier labs. Compare scenario deployment costs vs actual capex requirements.',
    'If artificial scarcity: suppression is high by design, beneficiaries are actively enforcing entry barriers, and the constraint reclassifies as high-suppression Snare. If genuine necessity: suppression is inherent to scaling laws, and the constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capex_concentration_lock_in, empirical, 'Whether capex concentration reflects genuine scaling necessity or artificial lock-in').

omega_variable(
    wage_deflation_mechanism,
    'Is the observed labor productivity stagnation despite AI adoption driven by real productivity substitution (AI replacing high-wage work) or by measurement failure to capture real gains?',
    'Sector-by-sector wage analysis: track wage growth for job categories with high vs low AI exposure. If high-exposure sectors show lower wage growth (substitution effect), extraction is real. If wage stagnation is global despite heterogeneous exposure, measurement is the issue.',
    'If substitution: labor is a structural victim, suppression is driven by labor market power imbalance, and the constraint reclassifies toward Snare at the labor perspective. If measurement: the victimization is statistical artifact and extraction narrative fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_deflation_mechanism, empirical, 'Whether AI adoption drives wage deflation through substitution or measurement artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_capex_productivity_puzzle, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aicapex_tr_t0, ai_capex_productivity_puzzle, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aicapex_tr_t2, ai_capex_productivity_puzzle, theater_ratio, 2, 0.58).
narrative_ontology:measurement(aicapex_tr_t4, ai_capex_productivity_puzzle, theater_ratio, 4, 0.65).
narrative_ontology:measurement(aicapex_tr_t1, ai_capex_productivity_puzzle, theater_ratio, 1, 0.52).
narrative_ontology:measurement(aicapex_tr_t3, ai_capex_productivity_puzzle, theater_ratio, 3, 0.62).

% Extraction over time
narrative_ontology:measurement(aicapex_be_t0, ai_capex_productivity_puzzle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(aicapex_be_t2, ai_capex_productivity_puzzle, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(aicapex_be_t4, ai_capex_productivity_puzzle, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(aicapex_be_t1, ai_capex_productivity_puzzle, base_extractiveness, 1, 0.36).
narrative_ontology:measurement(aicapex_be_t3, ai_capex_productivity_puzzle, base_extractiveness, 3, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_capex_productivity_puzzle, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_capex_productivity_puzzle, 0.12).
narrative_ontology:affects_constraint(ai_capex_productivity_puzzle, frontier_ai_research_funding).
narrative_ontology:affects_constraint(ai_capex_productivity_puzzle, labor_wage_growth_stagnation).
narrative_ontology:affects_constraint(ai_capex_productivity_puzzle, semiconductor_manufacturing_bottleneck).
narrative_ontology:affects_constraint(ai_capex_productivity_puzzle, open_source_ai_sustainability).

% DUAL FORMULATION NOTE:
% The AI capex-productivity puzzle decomposes into two constraints: AI_Frontier_Capability_Coordination (ε≈0.15, Rope — genuine research progress) and AI_Capex_Productivity_Paradox (ε≈0.55, Tangled Rope — macroeconomic extraction and measurement degradation). The first is upstream; the second is downstream. The puzzle's apparent contradiction vanishes when separated: capability coordination requires scaling (genuine Rope); capex concentration does not. Current institutional coupling conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_capex_productivity_puzzle, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
