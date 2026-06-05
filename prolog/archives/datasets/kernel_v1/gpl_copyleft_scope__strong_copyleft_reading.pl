% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft: Code Coupling Scope Extends to All Dynamic Linking
 *   domain: software_licensing/open_source_governance/intellectual_property
 *
 * SUMMARY:
 *   GPL Section 2(b) establishes the strong copyleft reading: any derivative
 *   work (including dynamically linked code, interpreted code executed in GPL
 *   interpreter runtime, or code coupled through inter-process communication
 *   with GPL software) must be distributed under GPL terms. This constraint
 *   instantiates ONE reading of a contested kernel: the definition of
 *   'derivative work' and the scope of copyleft obligations. The strong
 *   copyleft reading maximizes the scope — pulling in all runtime couplings,
 *   not just static linking. From the perspective of proprietary vendors,
 *   this is a Snare: they are structurally excluded from integrating GPL
 *   components without releasing source. From the perspective of GPL
 *   maintainers and free-software communities, it is coordination (Rope) or
 *   structural guarantee (Scaffold). The constraint's extractiveness (0.68)
 *   reflects the high cost imposed on proprietary strategies; its suppression
 *   (0.72) reflects credible enforcement through litigation and licensing
 *   audits. The theater ratio (0.38) indicates the licensing terms are
 *   explicit and functional, not performative — GPL enforcement is grounded
 *   in documented code analysis, not ritual or obscurity.
 *
 * KEY AGENTS:
 *   - Proprietary Software Vendors: Primary victim (powerless/trapped) — cannot integrate GPL components without full source release; zero exit options within proprietary strategy
 *   - Commercial Dual-License Firms: Secondary victim (moderate/constrained) — can purchase proprietary license but at high cost; face licensing audit risk
 *   - GPL-Licensed Project Maintainers: Primary beneficiary (institutional/arbitrage) — structurally benefit from copyleft scope that pulls downstream code into GPL terms; have relicense and dual-license options
 *   - Free Software Foundation: Organized enforcer (organized/constrained) — enforces strong copyleft scope; sees it as temporary mechanism to establish open-source norms before proprietary licensing becomes economically irrational
 *   - Open Source Developer Communities: Beneficiary with agency (organized/mobile) — benefit from code availability and forking rights; can choose which projects to contribute to
 *   - Large Technology Companies: Mixed position (powerful/mobile) — can fund GPL projects, contribute upstream, or develop alternatives; experience mixed coordination and extraction depending on portfolio strategy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing strong copyleft scope as inherent to GPL or open-source logic; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.72).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft: Code Coupling Scope Extends to All Dynamic Linking").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/open_source_governance/intellectual_property").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '7e209139-9ac2-44e7-af56-268d5e5122fe').
narrative_ontology:cs_kernel_codification('7e209139-9ac2-44e7-af56-268d5e5122fe', formalized).
narrative_ontology:cs_authority_grounding('7e209139-9ac2-44e7-af56-268d5e5122fe', lineage).
narrative_ontology:cs_interpretation_layer_present('7e209139-9ac2-44e7-af56-268d5e5122fe').
narrative_ontology:cs_reading_relation('7e209139-9ac2-44e7-af56-268d5e5122fe', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e209139-9ac2-44e7-af56-268d5e5122fe', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('7e209139-9ac2-44e7-af56-268d5e5122fe', foundational, derivative_work_includes_runtime_coupling).
narrative_ontology:cs_axiom_status(derivative_work_includes_runtime_coupling, holdable).
narrative_ontology:cs_axiom_grounding('7e209139-9ac2-44e7-af56-268d5e5122fe', derivative_work_includes_runtime_coupling, empirically_contingent).
narrative_ontology:cs_axiom('7e209139-9ac2-44e7-af56-268d5e5122fe', foundational, copyleft_scope_breadth_necessary_for_structural_guarantee).
narrative_ontology:cs_axiom_status(copyleft_scope_breadth_necessary_for_structural_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('7e209139-9ac2-44e7-af56-268d5e5122fe', copyleft_scope_breadth_necessary_for_structural_guarantee, instrumental).
narrative_ontology:cs_reference_frame('7e209139-9ac2-44e7-af56-268d5e5122fe', strong_copyleft_interpretation).
narrative_ontology:cs_drift_state('7e209139-9ac2-44e7-af56-268d5e5122fe', contemporary_cloud_computing_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7e209139-9ac2-44e7-af56-268d5e5122fe', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, derivative_work_recipients).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, closed_source_integrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPRIETARY VENDOR (SNARE) — Trapped by strong copyleft scope that extends to all runtime couplings. Vendor cannot integrate GPL component into proprietary workflow without full source release. Exit options are zero: abandon the component or abandon proprietary strategy. Suppression is structural (licensing enforcement, litigation threat). Maximum experienced extraction — beneficiary captures all strategic value from mandatory source release.
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL DUAL-LICENSE FIRM (SNARE) — Can exit via purchasing proprietary license, but at high cost (often 10-50% of revenue for dual-licensing). Faces suppression through licensing audit risk and copyright claim credibility. Experiences significant but not maximal extraction — has a costly exit (constrained) that proprietary vendors lack (trapped).
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GPL-LICENSED PROJECT MAINTAINER (ROPE) — Benefits structurally from strong copyleft scope: any integration pulls downstream code into GPL license terms. Experiences the constraint as pure coordination: the licensing mechanism enforces source availability, enabling contributor ecosystem. Net beneficiary with arbitrage options (relicense, dual-license, fork). No experienced extraction — the constraint subsidizes the maintainer's strategic position.
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FREE SOFTWARE FOUNDATION (SCAFFOLD) — Organized agent with enforcement power. Sees strong copyleft as a temporary coercive mechanism to establish open-source norms and code sharing practices. FSF's actual strategic goal is structural transition to world where proprietary licensing becomes economically irrational (open collaboration outcompetes closed development). Scaffold because enforcement is intentionally designed with time limit: as open-source dominance increases, copyleft coercion becomes less necessary. Theater ratio low (explicit licensing terms, clear enforcement path, not performative).
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE TECH COMPANY (TANGLED ROPE) — Powerful actor with mobile exit options: can fund GPL-licensed projects, employ GPL maintainers, contribute upstream, or develop non-GPL alternatives. Experiences the constraint as mixed coordination (enforces code sharing within their ecosystem) and extraction (some proprietary components are forced to GPL or abandoned). Both benefits and costs, both agency and constraint. Classification depends on specific company's GPL portfolio and strategy.
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SOURCE DEVELOPER COMMUNITY (ROPE) — Organized beneficiary with mobile exit options (choose which projects to contribute to, use alternatives). Experiences strong copyleft as pure coordination: it enables code sharing and forking rights, solving collective action problem of preventing proprietary capture. The licensing mechanism is functional (not performative) and solves the specific problem (code availability).
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the copyleft scope might appear as inherent to GPL's foundational logic: IF code is coupled at runtime, THEN it must be covered by the same terms. This perspective risks naturalizing what is actually a contested legal and technical interpretation (the definition of 'derivative work' and 'linking'). The engine's false summit detector will identify this as FSM candidate: GPL scope is a choice made by drafters and beneficiaries, not a law of nature.
constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_copyleft_scope__strong_copyleft_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The strong copyleft reading imposes severe cost on proprietary integrators — they must either release source (eliminating proprietary value) or abandon the GPL component entirely (losing functionality). The cost is not maximal (0.95) because vendors retain the exit option of using alternative (non-GPL) libraries or proprietary equivalents, but these alternatives often have lower quality, slower development, or higher licensing costs. Over the interval (1991-2016), extractiveness increased from 0.52 to 0.68 as the GPL components became more functionally critical (Linux, gcc, Apache became core infrastructure) and vendor lock-in increased. Suppression (0.72): High and increasing. Enforcement mechanisms include: copyright litigation (credible legal precedent), licensing audits (credible threat of damages), source code analysis tools (detection is nearly certain), and community enforcement (social pressure on non-compliant vendors). The suppression rose from 0.45 (1991, weak enforcement infrastructure) to 0.72 (2006+, mature enforcement ecosystem with litigation precedents and audit tools). Theater ratio (0.38): Low and decreasing. GPL enforcement is not performative — it is grounded in documented code analysis, clear licensing terms, and credible legal precedent. The theater ratio decreased from 0.55 (1991, when scope interpretation was emerging) to 0.38 (2006+, when scope became standardized and enforcement mechanisms were explicit). The licensing mechanism is functional: it achieves the stated goal of code availability through structural incentive, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   The strong copyleft reading produces maximum perspectival gap. The proprietary vendor sees a Snare (trapped, maximum extraction). The GPL maintainer sees a Rope (coordination mechanism). The FSF sees a Scaffold (temporary coercion with sunset). The large tech company sees Tangled Rope (mixed benefits and constraints). The community sees Rope (pure coordination benefit). The analytical observer risks seeing a Mountain (natural law inherent to open-source logic). This gap is not noise — it reveals the real structural feature: the constraint's effect on an agent depends entirely on their strategic relationship to source code release. If your business model depends on proprietary code, the constraint is extractive (Snare). If your business model depends on open collaboration, it is coordinative (Rope). The perspectives are not subjective opinions; they are structural measurements of extracted value relative to each agent's position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from: (1) beneficiary/victim status (is this agent the intended beneficiary of the licensing mechanism, or an unintended target?), (2) power level (what institutional or individual capacity does the agent have to organize alternative strategies?), (3) exit options (can this agent exit the constraint at acceptable cost?). Proprietary vendors are victims (not beneficiaries) with no exit, so d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.96 (maximum extraction experienced). GPL maintainers are beneficiaries with arbitrage options (relicense, dual-license), so d ≈ 0.05 → f(d) ≈ -0.12 → χ ≈ -0.08 (negative extraction, subsidy). Organized communities are beneficiaries with mobile options, so d ≈ 0.20 → f(d) ≈ 0.02 → χ ≈ 0.01 (minimal extraction). Large tech companies are mixed (both beneficiary and victim depending on portfolio), so d ≈ 0.48 → f(d) ≈ 0.60 → χ ≈ 0.41 (moderate extraction). The FSF enforcer (organized/constrained but deliberately coercive) has d ≈ 0.40 → f(d) ≈ 0.40 (moderate extraction, but intentional and time-limited). The analytical observer (observer/analytical) has canonical d ≈ 0.73 → f(d) ≈ 1.15 → χ ≈ 0.78 (high observed extraction, but this is the observation platform artifact, not the constraint's intrinsic structure).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between the constraint's intrinsic type (what it functionally does) and the perspectives' classifications (how different agents experience it). Intrinsically, the constraint functions as both coordination (enables code sharing, solves collective action problem against proprietary capture) and extraction (imposes cost on proprietary integrators). The engine's Snare and Rope classifications are not contradictory — they describe different directions of the same constraint relative to different agents. The mandatrophy resolution: the constraint's actual type is Tangled Rope (has both coordination and extraction dimensions), but because the extraction and coordination benefits flow to opposite agent classes, each agent experiences it as a single type (either pure coordination if they benefit, or pure extraction if they are targeted). The analytical observer's Mountain is a false summit — it naturalizes what is a deliberately designed institutional choice. The FSF's Scaffold classification is strategic: they intend the coercive mechanism to be temporary, lasting only until open-source development becomes economically dominant. The constraint's true type depends on the time horizon: it is Snare today (strong extraction from proprietary vendors), but Scaffold strategically (temporary mechanism with planned sunset as norms shift). The Tangled Rope classification at the large-tech-company perspective reveals the underlying structure: mixed coordination and extraction, with variable distribution across the agent ecosystem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_definition,
    'What constitutes a derivative work under copyright law? Does dynamic linking create derivative works, or merely interface dependencies?',
    'Legal case law precedent; technical analysis of linking mechanisms and memory model implications; jurisdictional variance in copyright interpretation (US vs EU vs international arbitration)',
    'Narrow scope: dynamic linking ≠ derivative work → constraint reclassifies from Snare (ε≈0.68) to Rope (ε≈0.25). Strong scope: all runtime couplings create derivative works → constraint remains Snare. This omega is THE core reading distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_definition, empirical, 'Whether dynamic linking creates copyright derivative works under applicable law').

omega_variable(
    enforcement_credibility_and_litigation_costs,
    'How credible are GPL enforcement threats against proprietary vendors? What are actual litigation costs and success rates for copyleft enforcement actions?',
    'Analysis of historical GPL litigation (BusyBox, Cisco, et al); survey of licensing audit threats and actual settlement rates; comparison of enforcement costs to potential damages',
    'If enforcement is primarily theater (low success rate, high vendor defense costs): constraint reclassifies from Snare (suppression=0.72) toward Piton (theater_ratio increases). If enforcement is credible (high settlement rates, clear precedent): constraint remains Snare with strong suppression signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_credibility_and_litigation_costs, empirical, 'Credibility of GPL enforcement via litigation and licensing audit threats').

omega_variable(
    commercial_code_sharing_convergence,
    'Are proprietary vendors being structurally pushed toward voluntary code sharing (not because of copyleft coercion, but because open collaboration is economically dominant)?',
    'Trend analysis of proprietary vendor open-source contributions; comparison of development velocity and feature quality (proprietary vs open-source) for similar tools; market share shift toward open-source alternatives',
    'If convergence is occurring: scaffold classification is correct (time-limited coercion), and the strong copyleft reading''s suppression mechanism is becoming less necessary. If vendors maintain robust proprietary-only strategies: strong copyleft suppression remains structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_code_sharing_convergence, empirical, 'Whether proprietary vendors are voluntarily converging toward open-source code sharing').

omega_variable(
    interpretation_layer_stability,
    'How stable is the GPL maintainer interpretation of ''derivative work'' scope? Are GPL maintainers and FSF enforcing a consistent, coherent boundary, or does interpretation drift with enforcement priorities?',
    'Analysis of GPL licensing decisions across projects (which projects enforce broad scope, which accept narrow scope); comparison of licensing decisions over time within the same project; interview/documentation of maintainer interpretation rationale',
    'If stable: the strong copyleft reading has predictable scope and classification. If drifting: the constraint''s effective suppression varies by project, and the Snare classification is more accurately Tangled Rope (with variable suppression across the ecosystem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_layer_stability, empirical, 'Consistency and stability of GPL scope interpretation by maintainers and enforcement bodies').

omega_variable(
    false_summit_fsm_candidate,
    'Is strong copyleft scope a natural law inherent to GPL''s logical structure, or a contingent institutional choice that benefits free-software communities?',
    'Comparison with alternative licensing schemes (LGPL, MPL, Apache) that achieve code-sharing goals with different scope boundaries; analysis of which reading (strong vs narrow scope) was explicit design choice vs downstream interpretation',
    'If natural law: mountain classification is appropriate. If contingent institutional choice: FSM triggers, reclassifies to Snare (high-epsilon extractive mechanism with identifiable beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_fsm_candidate, conceptual, 'Whether strong copyleft scope is inherent or contingent on GPL design choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2016).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_strong_theater_1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(gpl_strong_theater_1999, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(gpl_strong_theater_2006, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(gpl_strong_theater_2016, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(gpl_strong_extractiveness_1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gpl_strong_extractiveness_1999, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(gpl_strong_extractiveness_2006, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(gpl_strong_extractiveness_2016, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_strong_suppression_1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gpl_strong_suppression_1999, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(gpl_strong_suppression_2006, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(gpl_strong_suppression_2016, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, proprietary_lock_in_dynamics).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, open_source_sustainability_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the GPL copyleft scope kernel. The strong_copyleft_reading (ε=0.68, Snare) interprets derivative work scope broadly. The narrow_scope_reading (sibling, separate story) interprets scope narrowly and produces ε≈0.20 (Rope). The enforcement_vacuum_reading (sibling, separate story) treats scope as written but enforcement as impossible, producing ε≈0.25 (Piton). All three share the same GPL text (fixed kernel) but interpret it differently. They are linked via network.affects_constraints to show the interpretive divergence. Each story is ε-invariant within its reading — extractiveness does not vary when you change the observable, it varies when you change the INTERPRETATION of what the licensing scope covers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, institutional, 0.08).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
