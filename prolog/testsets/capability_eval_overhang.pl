% ============================================================================
% CONSTRAINT STORY: capability_eval_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_eval_overhang, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capability_eval_overhang
 *   human_readable: The Blind Spot of Power: AI Capability Evaluation Overhang
 *   domain: technological/AI/governance
 *
 * SUMMARY:
 *   The capability evaluation overhang represents a structural asymmetry
 *   where frontier AI system capabilities significantly exceed the benchmarks
 *   and evaluation protocols used to assess them. This creates a blind spot
 *   in governance: regulators, policymakers, and competing organizations make
 *   decisions based on disclosed metrics that systematically understate true
 *   capabilities. The constraint operates across multiple institutional
 *   levels — from individual model release decisions by frontier labs, to
 *   funding and partnership allocation by competitive organizations, to
 *   regulatory posture by governance institutions, to research prioritization
 *   by the academic community. The overhang is sustained by incentive
 *   asymmetries (developers benefit from maintaining uncertainty about true
 *   capabilities), information asymmetries (only developers have access to
 *   model internals), and structural barriers to verification (evaluating
 *   frontier capability requires expensive, specialized red-teaming and
 *   access to internals). The theater ratio (0.64) reflects that published
 *   benchmarks maintain ritualistic status despite acknowledged limitations —
 *   researchers and policymakers continue to cite and rely on benchmark
 *   scores even while noting their insufficiency.
 *
 * KEY AGENTS:
 *   - Frontier AI Labs: Primary beneficiary (institutional/arbitrage) — capture extended first-mover advantage, funding concentration, and licensing leverage from prolonged capability uncertainty
 *   - Governance Institutions: Primary victim (powerless/trapped) — lack independent verification capacity, must regulate based on disclosed metrics, bear political cost of unknown capability gaps
 *   - Competitive Organizations: Secondary victim (moderate/constrained) — face misdirected strategy decisions based on underestimated frontier capability; cannot freely exit the benchmark-based comparison framework
 *   - Public Epistemic Commons: Secondary victim (powerless/trapped) — scientific understanding of AI capabilities is systematically biased toward frontier-lab disclosed metrics; resource allocation for safety research is distorted
 *   - Research Community: Mixed position (organized/constrained) — benefits from access to frontier models (coordination function) but research direction is misdirected by obscured true capability distribution
 *   - Evaluation Infrastructure: Institutional actor (institutional/arbitrage) — traditional benchmarks persist through inertia despite known limitations; maintains theater of verification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (developer secrecy, regulatory access gaps) as inherent properties of AI capability advancement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_eval_overhang, 0.58).
domain_priors:suppression_score(capability_eval_overhang, 0.68).
domain_priors:theater_ratio(capability_eval_overhang, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_eval_overhang, extractiveness, 0.58).
narrative_ontology:constraint_metric(capability_eval_overhang, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(capability_eval_overhang, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_eval_overhang, snare).
narrative_ontology:human_readable(capability_eval_overhang, "The Blind Spot of Power: AI Capability Evaluation Overhang").
narrative_ontology:topic_domain(capability_eval_overhang, "technological/AI/governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_eval_overhang, ai_capability_developers).
narrative_ontology:constraint_beneficiary(capability_eval_overhang, frontier_labs).
narrative_ontology:constraint_victim(capability_eval_overhang, governance_institutions).
narrative_ontology:constraint_victim(capability_eval_overhang, public_epistemic_commons).
narrative_ontology:constraint_victim(capability_eval_overhang, competitive_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOVERNANCE INSTITUTION (SNARE) — Regulators and policymakers cannot exit the evaluation regime. They possess no independent capability benchmarks, no real-time access to model internals, and depend entirely on developers' disclosed metrics. Trapped between public demand for safety assurance and inability to verify claims. Maximum experienced extraction — the institution bears the cost of unknown unknowns while competitors and developers capture asymmetric advantage.
constraint_indexing:constraint_classification(capability_eval_overhang, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETITIVE ORGANIZATION (SNARE) — Smaller AI labs and non-frontier organizations face constrained exits. Benchmarks published by frontier labs become reference frames for funding, hiring, and partnership decisions. The overhang creates information asymmetry: frontier labs know their true capability gaps; competitors do not. Significant extraction as competitive organizations make strategy decisions based on underestimated frontier capability.
constraint_indexing:constraint_classification(capability_eval_overhang, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER AI LAB (ROPE) — Benefits from the evaluation overhang through extended first-mover advantage, licensing leverage, and capital accumulation. Experiences the constraint as coordination: publishing benchmark results coordinates expectations among investors, researchers, and policymakers. Net beneficiary with high exit options (can threshold disclosure, can select which benchmarks to publicize). Low experienced extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(capability_eval_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (TANGLED ROPE) — Academic researchers and benchmarking consortiums benefit from published model outputs (coordination function) but also lose ability to allocate research effort efficiently when true capability distribution is obscured. The constraint has both coordination value (access to frontier models enables downstream research) and extraction cost (misdirected research effort, publication bias toward models appearing weaker than they are). Constrained exit — researchers depend on published benchmarks for grant decisions and publication pathways.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EVALUATION INFRASTRUCTURE (PITON) — Traditional benchmarking frameworks (MMLU, ARC, HumanEval, etc.) persist as reference points despite known limitations. The infrastructure's functional verification capacity has atrophied as models exceed the designers' original capability assumptions. Benchmarks remain in use through institutional inertia and coordination on familiar metrics rather than because they effectively measure true capability. Theater ratio high: ritualistic benchmark reporting continues even when practitioners know the metrics are insufficient.
constraint_indexing:constraint_classification(capability_eval_overhang, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational vantage, some evaluation lag is inherent to rapid capability advancement: verification always lags development by necessity. No organizational structure can guarantee real-time capability assessment of systems designed for optimization. This perspective risks naturalizing the overhang as an immutable property of AI development itself. However, the structural data contradicts pure mountain classification — the overhang is sustained by incentive asymmetries (developers benefit from delayed disclosure, regulators lack access), not by laws of nature.
constraint_indexing:constraint_classification(capability_eval_overhang, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_eval_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_eval_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_eval_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_eval_overhang, TR),
    TR >= 0.70.

:- end_tests(capability_eval_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts value from trapped and constrained agents through prolonged information asymmetry. Frontier labs capture extended licensing leverage, capital accumulation, and competitive positioning. However, extractiveness is not maximal (0.80+) because the overhang eventually decays as true capabilities are revealed through deployment, independent testing, and competitive release. The trajectory shows increasing extractiveness over time (0.35 → 0.58 over 6 time units) as the capability gap widens and is increasingly observed but not acknowledged. Suppression (0.68): High. Multiple barriers prevent verification: computational costs of independent evaluation, IP protection preventing model access, lack of government sandboxing infrastructure, and tacit knowledge in model training. Governance institutions and competitors have extremely constrained options for independent capability assessment. Theater ratio (0.64): Moderate-high. Benchmark publication maintains ritual status despite widespread recognition that metrics are insufficient. Practitioners continue citing MMLU, ARC, and other scores as though they represent comprehensive capability assessment, even while noting the gaps. The theater persists because no coordinated alternative infrastructure exists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the gap between true capability and disclosed metrics — appears radically different from different structural positions. The frontier lab sees coordination (Rope): publishing benchmarks coordinates expectations among investors and researchers, enabling capital formation and talent recruitment. The governance institution sees extraction without alternatives (Snare): it must regulate without verification capacity, bearing political cost for unknown risks. The competitive organization sees partial extraction (Snare with constrained mobility): it can fund independent evaluation but cannot match frontier lab's access to its own internals. The research community sees a mixed constraint (Tangled Rope): they benefit from access to frontier models but suffer misdirected effort. The evaluation infrastructure sees its own degraded ritual (Piton): benchmarks persist through institutional inertia despite acknowledged limitations. The analytical observer risks seeing an immutable natural law (Mountain): capability advancement inherently outpaces verification — but the structural data reveals this as a false summit. The overhang is sustained by contingent institutional factors (IP protection, lack of government access rights, absence of sandboxing infrastructure), not by laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's position relative to the information asymmetry. Frontier labs hold perfect information about their true capabilities while others possess only disclosed metrics — this creates a d ≈ 0.05 for labs (full beneficiaries with arbitrage options: they can threshold disclosure, select which benchmarks to publicize, time releases strategically). Governance institutions have no independent verification capacity and cannot exit the regulatory requirement — d ≈ 0.95 (full targets, trapped). Competitive organizations can observe some capability signals through deployment but lack access to internals — d ≈ 0.75 (victims with slight mobility through independent testing). The research community benefits from access to frontier models but suffers misdirected effort from obscured capability distribution — d ≈ 0.60 (asymmetric: some benefit, some cost). The evaluation infrastructure has arbitrage options (can maintain benchmarks or fund new metrics) but is constrained by coordination costs — d ≈ 0.35 (moderate victim). The pipeline computes f(d) for each context and applies scope modifiers (global scope σ = 1.2 amplifies extractiveness).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification as Snare (primary) is legitimate and not a mislabeling of coordination as extraction. The coordination function is minimal and asymmetrically distributed: frontier labs benefit from publishing benchmarks, but this is not a genuine coordination mechanism that benefits all parties. The asymmetric extraction is clear: governance institutions and competitors lose by having their decisions distorted by underestimated capability. The active enforcement is implicit (developers choose what to disclose, when to disclose, which benchmarks to publicize — this is a unilateral choice structure, not genuinely negotiated). The key mandatrophy resolution: Does the overhang serve a genuine coordination purpose (justifying Tangled Rope classification), or is it pure extraction (Snare)? The evidence points to Snare: the information asymmetry could be reduced through transparency mechanisms without eliminating genuine coordination benefits. The persistence of the overhang reflects developer incentives to maintain uncertainty, not technical necessity for coordination. If governance institutions had real-time access to model internals, the coordination benefits (building safe models, publishing results) would persist, but the extraction mechanism would collapse. This confirms Snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_capability_distribution,
    'What is the actual distribution of frontier AI capability across domains, and how does it compare to published benchmark results?',
    'Independent third-party access to model internals; comprehensive capability audits across disclosed and undisclosed domains; red-teaming against actual system constraints',
    'If true capability is 1.5-2.0x benchmark results: overhang is moderate, extraction timeline is 2-5 years. If true capability is 3.0x+: overhang is severe, extraction timeline extends to 10+ years. If true capability matches published benchmarks: constraint reclassifies from Snare to Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_capability_distribution, empirical, 'True distribution of frontier AI capabilities versus published benchmarks').

omega_variable(
    benchmark_gaming_detection,
    'To what degree are frontier models optimized specifically to maximize benchmark scores versus developing general capability?',
    'Analysis of training objectives and fine-tuning signals; out-of-distribution capability testing; comparison of benchmark performance to real-world task performance in blind evaluations',
    'If optimization is 20-30%: overhang reflects genuine capability gap. If optimization is 60%+: the benchmark gap is partially artifact; the extraction mechanism is misrepresented as capability asymmetry when it is partially gaming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_gaming_detection, empirical, 'Degree of explicit benchmark optimization versus general capability development').

omega_variable(
    governance_access_feasibility,
    'Can regulatory institutions obtain sufficient real-time access to frontier model internals without compromising IP or operational security?',
    'Technical feasibility studies on interpretability, sandboxing, and red-teaming protocols; policy experiments with sandboxed model access; comparison to pharmaceutical FDA inspection regimes',
    'If feasible: constraint can transition from Snare to Tangled Rope through enforcement mechanisms. If infeasible: governance remains trapped, extraction persists, constraint reclassifies toward more pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_access_feasibility, empirical, 'Technical and institutional feasibility of real-time regulatory access to frontier models').

omega_variable(
    acceleration_inevitability,
    'Is the evaluation overhang an artifact of current institutional immaturity, or is it an inevitable consequence of open-ended AI capability advancement?',
    'Historical analysis of capability growth rates; comparison to pharmaceutical/nuclear development cycles; modeling of verification timelines under different institutional structures',
    'If artifact: constraint can be resolved through coordination and transparency mechanisms (Scaffold perspective validated). If inevitable: overhang is structural to capability advancement (Mountain perspective gains force).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(acceleration_inevitability, conceptual, 'Whether evaluation overhang is institutional or inevitable feature of capability advancement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_eval_overhang, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capeval_tr_t0, capability_eval_overhang, theater_ratio, 0, 0.48).
narrative_ontology:measurement(capeval_tr_t3, capability_eval_overhang, theater_ratio, 3, 0.58).
narrative_ontology:measurement(capeval_tr_t6, capability_eval_overhang, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(capeval_be_t0, capability_eval_overhang, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(capeval_be_t3, capability_eval_overhang, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(capeval_be_t6, capability_eval_overhang, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_eval_overhang, information_standard).
narrative_ontology:affects_constraint(capability_eval_overhang, ai_governance_asymmetry).
narrative_ontology:affects_constraint(capability_eval_overhang, benchmark_optimization_gaming).
narrative_ontology:affects_constraint(capability_eval_overhang, capability_disclosure_lag).

% DUAL FORMULATION NOTE:
% The capability evaluation overhang is upstream of specific governance failures (misallocated safety investment, inadequate regulatory posture) but represents a distinct structural constraint. The overhang enables downstream extractive dynamics in licensing, deployment timing, and competitive positioning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capability_eval_overhang, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
