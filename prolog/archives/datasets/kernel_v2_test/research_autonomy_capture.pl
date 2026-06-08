% ============================================================================
% CONSTRAINT STORY: research_autonomy_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_research_autonomy_capture, []).

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
 *   constraint_id: research_autonomy_capture
 *   human_readable: Research Autonomy Capture via AI Tool Dependency
 *   domain: science_policy/professional_ethics/technology_governance
 *
 * SUMMARY:
 *   Research autonomy capture describes the structural shift in academic
 *   research priorities from expert-judged intellectual significance toward
 *   problems amenable to automated AI methods. This constraint operates
 *   through funding allocation mechanisms, hiring criteria, and publication
 *   venue selection that increasingly privilege computational approaches and
 *   AI tool proficiency regardless of whether the research questions
 *   genuinely require or benefit from these methods. The constraint exhibits
 *   genuine coordination function — AI tools do accelerate certain research
 *   tasks and enable new problem classes — but also asymmetric extraction:
 *   early-career researchers face rising barriers to entry through compute
 *   access requirements, theory-driven programs must justify themselves in
 *   computational terms to secure funding, and epistemic authority shifts
 *   from domain experts to platform providers. The theater_ratio (0.48)
 *   reflects growing performative adoption of AI methods to satisfy funding
 *   requirements even when the tools add minimal intellectual value to the
 *   research question. Suppression (0.62) captures the structural barriers:
 *   proprietary tool dependencies, compute cost concentration, and
 *   hiring/funding criteria that filter for vendor alignment rather than
 *   autonomous research capability.
 *
 * KEY AGENTS:
 *   - Technology Companies / AI Tool Vendors: Primary beneficiary (institutional/arbitrage) — capture licensing revenue, data access rights, and platform lock-in through academic partnerships; establish proprietary methods as field standards
 *   - Early-Career Researchers Without Compute: Primary victim (powerless/trapped) — face insurmountable barriers to entry through compute access requirements and hiring criteria that mandate AI tool proficiency; cannot exit to alternative research pathways
 *   - Theory-Driven Research Programs: Secondary victim (moderate/constrained) — must justify research in computational terms to secure funding even when core questions are conceptual; can pivot to hybrid approaches but at cost of abandoning autonomous directions
 *   - Autonomous Mathematical Research: Abstract victim (powerless/trapped) — research directions that resist automation or require deep conceptual work are systematically defunded and deprioritized; no advocate and no exit
 *   - Open-Source AI Coalition: Organized agents (organized/mobile) — building open-weight models and community compute access to break proprietary capture; see current extraction as transitional with sunset logic
 *   - University Research Administration: Institutional actor (institutional/constrained) — benefits from AI partnership revenue but constrained by asymmetric terms and reputational risk from reproducibility failures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (tools do accelerate some research) and asymmetric extraction (problem selection distorted, autonomy eroded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(research_autonomy_capture, 0.58).
domain_priors:suppression_score(research_autonomy_capture, 0.62).
domain_priors:theater_ratio(research_autonomy_capture, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(research_autonomy_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(research_autonomy_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(research_autonomy_capture, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(research_autonomy_capture, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(research_autonomy_capture, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(research_autonomy_capture, tangled_rope).
narrative_ontology:human_readable(research_autonomy_capture, "Research Autonomy Capture via AI Tool Dependency").
narrative_ontology:topic_domain(research_autonomy_capture, "science_policy/professional_ethics/technology_governance").

domain_priors:requires_active_enforcement(research_autonomy_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(research_autonomy_capture, technology_companies).
narrative_ontology:constraint_beneficiary(research_autonomy_capture, ai_tool_vendors).
narrative_ontology:constraint_beneficiary(research_autonomy_capture, computational_research_groups).
narrative_ontology:constraint_victim(research_autonomy_capture, autonomous_mathematical_research).
narrative_ontology:constraint_victim(research_autonomy_capture, theory_driven_research_programs).
narrative_ontology:constraint_victim(research_autonomy_capture, early_career_researchers_without_compute).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped by funding requirements that mandate AI tool use and computational infrastructure they cannot afford. Career advancement requires demonstrating proficiency with proprietary tools and access to compute resources concentrated in well-funded labs. Cannot exit to alternative research pathways because hiring committees and grant panels now filter for AI methodology regardless of problem appropriateness. Maximum extraction — structural barriers to entry have risen dramatically.
constraint_indexing:constraint_classification(research_autonomy_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THEORY-DRIVEN PROGRAM (TANGLED ROPE) — Constrained by funding allocation shifts toward AI-amenable problems but also benefits from computational tools that accelerate certain verification tasks. Genuine coordination exists (AI tools do solve some problems faster) but extraction is asymmetric: must justify research programs in terms of AI applicability to secure funding, even when the core questions are conceptual rather than computational. Can pivot toward hybrid approaches but at the cost of abandoning some autonomous research directions.
constraint_indexing:constraint_classification(research_autonomy_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: providing tools that researchers need, capturing citation advantage and data access through academic partnerships, establishing proprietary methods as field standards. Net beneficiary — extraction flows toward this agent through licensing fees, data rights, and platform lock-in, but from their structural position this appears as legitimate value exchange.
constraint_indexing:constraint_classification(research_autonomy_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE COALITION (SCAFFOLD) — Organized agents building open-weight models, reproducible pipelines, and community compute access see the current proprietary capture as temporary. Sunset logic: as open-source alternatives mature and compute costs decline, the extraction mechanism loses force. Transitional coordination to break vendor lock-in and restore research autonomy through distributed infrastructure.
constraint_indexing:constraint_classification(research_autonomy_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNIVERSITY ADMINISTRATION (TANGLED ROPE) — Institutional actor that both benefits from and is constrained by the shift. Benefits: AI partnerships bring overhead revenue, industry funding, and prestige rankings. Constrained: must negotiate asymmetric terms with technology partners, faces pressure to redirect resources toward computational infrastructure at the expense of theory-heavy departments, and bears reputational risk when proprietary dependencies limit reproducibility. Mixed extraction — some institutional agency but significant structural pressure.
constraint_indexing:constraint_classification(research_autonomy_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, genuine coordination exists: AI tools do accelerate certain research tasks, enable new problem classes, and facilitate collaboration. But asymmetric extraction is also structural: problem selection is distorted toward AI-amenable questions regardless of intellectual significance, research autonomy erodes as funding and hiring criteria privilege tool use over conceptual depth, and epistemic authority shifts from domain experts to platform providers. Both functions coexist — this is the definitional tangled rope from the analytical seat.
constraint_indexing:constraint_classification(research_autonomy_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(research_autonomy_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(research_autonomy_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(research_autonomy_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(research_autonomy_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(research_autonomy_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Technology companies capture licensing revenue, data rights, and platform lock-in through academic partnerships. Early-career researchers face rising compute access barriers. Theory-driven programs must justify themselves in computational terms regardless of problem appropriateness. Problem selection shifts toward AI-amenable questions even when intellectual significance lies elsewhere. The value reflects that extraction is significant but not total — some researchers retain autonomy and some AI adoption is genuinely productive. Suppression (0.62): Substantial. Structural barriers include proprietary tool dependencies, compute cost concentration in well-funded labs, publication venue preferences for computational methods, hiring criteria that filter for AI tool proficiency, and funding panel composition favoring computational approaches. Alternatives exist (open-source tools, theory-focused venues) but face systematic disadvantage. Theater ratio (0.48): Moderate. Growing performative adoption of AI methods to satisfy funding requirements even when tools add minimal value. Grant applications include AI components for credibility signaling. Hiring committees privilege tool proficiency over conceptual depth. But theater is not yet dominant — substantial genuine use remains.
 *
 * PERSPECTIVAL GAP:
 *   The technology company sees pure coordination (Rope) — they provide tools researchers need and capture legitimate value through licensing. The early-career researcher without compute access sees pure extraction (Snare) — insurmountable barriers to entry and no exit to alternative pathways. The theory-driven program sees mixed coordination and extraction (Tangled Rope) — tools do accelerate some tasks but funding allocation distorts problem selection. The open-source coalition sees temporary extraction with sunset (Scaffold) — current capture will dissolve as open alternatives mature. The university administration sees mixed benefits and constraints (Tangled Rope) — AI partnerships bring revenue but impose asymmetric terms. The analytical observer sees definitional tangled rope — genuine coordination coexists with asymmetric extraction in the same structural mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies are primary beneficiaries with arbitrage exit options — they experience negative effective extraction (the constraint subsidizes them through licensing revenue, data access, and platform lock-in). Early-career researchers without compute are primary victims with trapped exit — they experience maximum extraction through insurmountable access barriers and hiring criteria they cannot satisfy. Theory-driven programs are secondary victims with constrained exit — they experience substantial extraction through funding distortion but retain some agency to pivot toward hybrid approaches. The open-source coalition has mobile exit options and organized power — they experience moderate extraction because they are building alternative pathways. University administrations are institutional actors with constrained exit — they experience mixed extraction because they benefit from partnership revenue but face asymmetric terms and reputational risk. The analytical observer has analytical exit and sees the full structure — both coordination and extraction are real and coexist.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a transitional state between rope and snare but a stable structural configuration where genuine coordination and asymmetric extraction coexist in the same mechanism. AI tools do provide real research value (coordination function) AND distort problem selection toward vendor interests (extraction function). The coordination is not cover for extraction, and the extraction is not incidental to coordination — both are structural features. The perspectival gap reveals this: beneficiaries see only coordination, trapped victims see only extraction, and the analytical observer sees both. The mandate (accelerate research through computational tools) coexists with extraction (capture research autonomy and redirect priorities toward AI-amenable problems). Neither function has outlived the other — they are coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tool_dependency_vs_capability_expansion,
    'Does AI tool adoption expand research capability or create dependency that constrains problem selection?',
    'Longitudinal analysis of problem diversity in AI-heavy vs theory-heavy research programs; comparison of research questions pursued before and after AI tool adoption; measurement of whether tools enable new questions or merely automate existing approaches',
    'If capability expansion dominates: coordination function is genuine and extraction is lower than measured. If dependency dominates: extraction is higher and the coordination story is cover for vendor lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tool_dependency_vs_capability_expansion, empirical, 'Whether AI tools expand capability or create constraining dependency').

omega_variable(
    open_source_sunset_timeline,
    'Will open-source alternatives mature fast enough to prevent permanent capture of research infrastructure?',
    'Tracking compute cost trajectories, open-weight model performance parity timelines, and institutional adoption rates of open vs proprietary tools; measurement of whether early-career researchers can access competitive tools without vendor relationships',
    'If open-source achieves parity within 5-7 years: scaffold perspective is structurally correct and extraction is transitional. If proprietary advantages persist beyond 10 years: capture becomes permanent institutional feature and scaffold perspective is aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_sunset_timeline, empirical, 'Timeline for open-source tools to break proprietary capture').

omega_variable(
    problem_significance_vs_amenability,
    'Are AI-amenable problems genuinely more significant, or is significance being redefined to match tool capabilities?',
    'Expert surveys comparing pre-AI and post-AI problem prioritization; analysis of whether funding follows intellectual significance or computational tractability; longitudinal tracking of which research directions are abandoned vs pursued',
    'If significance is being redefined: extraction is higher than measured because epistemic authority has shifted from domain experts to tool providers. If AI-amenable problems are genuinely more significant: coordination function is stronger and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(problem_significance_vs_amenability, conceptual, 'Whether problem significance is being redefined to match AI tool capabilities').

omega_variable(
    hiring_criteria_distortion,
    'Do hiring criteria emphasizing AI tool proficiency select for research capability or for vendor alignment?',
    'Analysis of hiring outcomes: correlation between AI tool proficiency and research productivity; comparison of career trajectories for tool-proficient vs theory-deep candidates; measurement of whether tool requirements filter for genuine capability or for willingness to work within proprietary ecosystems',
    'If tool proficiency correlates with capability: hiring criteria are legitimate coordination. If tool proficiency correlates with vendor alignment: hiring criteria are extraction mechanism that privileges commercial partnerships over autonomous research.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hiring_criteria_distortion, empirical, 'Whether hiring criteria select for capability or vendor alignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(research_autonomy_capture, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(res_auto_tr_t0, research_autonomy_capture, theater_ratio, 0, 0.22).
narrative_ontology:measurement(res_auto_tr_t2, research_autonomy_capture, theater_ratio, 2, 0.31).
narrative_ontology:measurement(res_auto_tr_t4, research_autonomy_capture, theater_ratio, 4, 0.39).
narrative_ontology:measurement(res_auto_tr_t6, research_autonomy_capture, theater_ratio, 6, 0.44).
narrative_ontology:measurement(res_auto_tr_t8, research_autonomy_capture, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(res_auto_be_t0, research_autonomy_capture, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(res_auto_be_t2, research_autonomy_capture, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(res_auto_be_t4, research_autonomy_capture, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(res_auto_be_t6, research_autonomy_capture, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(res_auto_be_t8, research_autonomy_capture, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(res_auto_su_t0, research_autonomy_capture, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(res_auto_su_t2, research_autonomy_capture, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(res_auto_su_t4, research_autonomy_capture, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(res_auto_su_t6, research_autonomy_capture, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(res_auto_su_t8, research_autonomy_capture, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(research_autonomy_capture, resource_allocation).

% DUAL FORMULATION NOTE:
% Research autonomy capture is downstream of attribution_erosion (which erodes credit assignment for AI-assisted work), review_system_collapse (which reduces quality control as AI-generated submissions overwhelm review capacity), and asymmetric_collaboration_terms (which establish unequal partnerships between academic researchers and technology companies). Each upstream constraint has its own extractiveness reflecting specific mechanisms; research_autonomy_capture represents the aggregate effect on research priority-setting and epistemic authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
