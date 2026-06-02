% ============================================================================
% CONSTRAINT STORY: algorithm_selection_in_institutional_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithm_selection_in_institutional_design, []).

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
 *   constraint_id: algorithm_selection_in_institutional_design
 *   human_readable: Algorithm Selection in Institutional Design
 *   domain: institutional_design/governance/technology_policy
 *
 * SUMMARY:
 *   Algorithm selection in institutional design creates a structural
 *   constraint where the scaling function (algorithms enable institutions to
 *   make decisions at scale) is genuine coordination, yet the selection and
 *   deployment of specific algorithms embeds asymmetric extraction of power
 *   and information. An institution deploying a hiring algorithm coordinates
 *   the scaling problem: evaluating thousands of candidates manually would be
 *   infeasible. But the specific algorithm chosen — the objectives it
 *   optimizes, the data it uses, the decisions it delegates vs flags for
 *   human review — concentrates authority, obscures decisions, and
 *   externalizes compliance costs onto the subjects of algorithmic decisions.
 *   The constraint exhibits a dual character: it solves a real coordination
 *   problem while simultaneously serving as an extraction mechanism.
 *   Extractiveness increased from 0.35 to 0.58 over the measurement interval
 *   as algorithmic complexity outpaced institutional transparency capacity
 *   and as systems accumulated proprietary components resistant to external
 *   audit. Theater ratio increased from 0.42 to 0.68, indicating that the
 *   justification for algorithmic deployment increasingly relies on
 *   performance theater (legitimacy through automation, implied objectivity)
 *   rather than demonstrated decision quality.
 *
 * KEY AGENTS:
 *   - Algorithmic Subjects: Primary victims (powerless/trapped) — individuals subject to algorithmic decisions in hiring, lending, benefit allocation, policing, with no exit, no transparency, no recourse
 *   - Algorithm Designers and Vendors: Primary beneficiaries (institutional/arbitrage) — firms selling algorithmic systems; benefit from first-mover advantage, regulatory capture potential, scaling revenue; have exit options (sell to different institutions, different markets)
 *   - Institutional Administrators: Secondary beneficiary/victim (institutional/constrained) — schools, courts, lenders deploying algorithms; benefit from scalability and delegation; constrained by regulatory requirements, liability for errors, public pressure
 *   - Advocacy Coalition: Secondary victim (moderate/constrained) — civil rights organizations, algorithmic justice advocates; have research access and participation opportunities; constrained by resource barriers and institutional access; benefit from accountability venues
 *   - Regulatory Reform Movement: Organized reformer (organized/constrained) — EU regulators, standards bodies, transparency advocates building alternative pathways with sunset mechanism; have agency and political power; constrained by industry resistance and implementation complexity
 *   - Legacy Decision Infrastructure: Institutional actor (institutional/arbitrage) — existing decision-making systems (hiring committees, credit review boards) that have been replaced by algorithmic versions; maintain algorithmic legitimacy through theater; see alternative decision pathways as inferior
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — sees the constraint's dual nature: genuine coordination coexisting with genuine extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithm_selection_in_institutional_design, 0.58).
domain_priors:suppression_score(algorithm_selection_in_institutional_design, 0.62).
domain_priors:theater_ratio(algorithm_selection_in_institutional_design, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithm_selection_in_institutional_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithm_selection_in_institutional_design, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithm_selection_in_institutional_design, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithm_selection_in_institutional_design, tangled_rope).
narrative_ontology:human_readable(algorithm_selection_in_institutional_design, "Algorithm Selection in Institutional Design").
narrative_ontology:topic_domain(algorithm_selection_in_institutional_design, "institutional_design/governance/technology_policy").

domain_priors:requires_active_enforcement(algorithm_selection_in_institutional_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithm_selection_in_institutional_design, algorithm_designers).
narrative_ontology:constraint_beneficiary(algorithm_selection_in_institutional_design, institutional_incumbents).
narrative_ontology:constraint_beneficiary(algorithm_selection_in_institutional_design, centralized_administrators).
narrative_ontology:constraint_victim(algorithm_selection_in_institutional_design, algorithmic_subjects).
narrative_ontology:constraint_victim(algorithm_selection_in_institutional_design, institutional_transparency).
narrative_ontology:constraint_victim(algorithm_selection_in_institutional_design, decentralized_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC SUBJECT (SNARE) — Individuals subject to algorithmic decisions in institutional contexts (loan denial, job screening, benefit allocation, policing) cannot exit the system or appeal the mechanism. Trapped by territorial jurisdiction and legal dependency. The algorithmic constraint operates with maximal extraction: asymmetric information, no transparency, no recourse, and no alternatives. Subject experiences the full force of suppression.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADVOCACY COALITION (TANGLED ROPE) — Civil rights organizations, algorithmic justice advocates, and community groups see a constraint with mixed dynamics. Genuine coordination function: algorithmic institutions do coordinate resource allocation and decision-making at scale. But active extraction embedded: the algorithms are designed in ways that obscure bias and concentrate power. Coalition is constrained by resource barriers and institutional access; benefits from having participation venues (if available) and research access. Mixed extraction and coordination.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM DESIGNER (ROPE) — Designers/vendors experience the constraint as pure coordination. The algorithm solves the scaling problem: without it, institutional decisions would require manual case-by-case review, which is infeasible for large systems. Designers have arbitrage options (sell to different institutions, choose different design parameters). Net beneficiary; experiences minimal extraction. The constraint is genuinely functional for this perspective.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL ADMINISTRATOR (TANGLED ROPE) — Administrators in institutions deploying algorithms (schools, courts, credit bureaus) benefit from scalability and delegation of decisions. But also constrained: regulatory requirements, liability for algorithmic errors, public pressure to explain opaque decisions, cost of maintaining systems. Coordination function (scaling decisions) genuine; extraction embedded (cost externalized to algorithmic subjects, opacity delegated upward to boards). Moderate extraction, some agency.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORM MOVEMENT (SCAFFOLD) — Organized actors (AI transparency advocates, regulators, standards bodies) see the algorithmic constraint as a temporary institutional failure with a regulatory sunset. EU AI Act, algorithmic audit mandates, transparency requirements, and explainability standards represent active efforts to decompose the constraint: extractive opacity replaced by mandatory disclosure, human review requirements, impact assessments. Suppression declining as standards mature. Organized agents see exit path.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY DECISION INFRASTRUCTURE (PITON) — The ritual of 'algorithmic justification' persists in many institutions not because algorithms actually improve decisions but because the performance of algorithmic authority maintains institutional legitimacy. Rule-of-three hiring committees have been replaced by ATS (applicant tracking systems) not primarily because ATS screens better but because it performs automation and impartiality. Theater ratio high (0.68): the algorithmic ritual persists through inertia, institutional prestige, and litigation liability management. Actual functional verification low.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical view, algorithm selection exhibits the hybrid character of institutional design itself: all scaling mechanisms embed asymmetric power. Genuine coordination function (scaling decisions) coexists with genuine extraction (concentration of authority, information asymmetry, compliance cost offloaded to subjects). No perspectival collapse — this is not a false summit. The constraint is structurally tangled: you cannot have scaled decision-making without some asymmetry, and you cannot eliminate asymmetry without accepting the costs of distributed review.
constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithm_selection_in_institutional_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithm_selection_in_institutional_design, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithm_selection_in_institutional_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithm_selection_in_institutional_design, TR),
    TR >= 0.70.

:- end_tests(algorithm_selection_in_institutional_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The base extractiveness reflects genuine asymmetries in algorithm selection: designers optimize for institutional efficiency and vendor profit, not algorithmic subject welfare. Information asymmetry is extreme (subjects don't know what data is used, what rules trigger decisions, whether decisions are even algorithmic or human). Subjects have no appeal mechanism, no transparency, no exit. However, extractiveness is not 0.80+ (snare-level) because many institutions do face regulatory pressure and some do invest in bias audits and transparency. The increasing trend (0.35→0.58) reflects accumulation of proprietary components, growing complexity outpacing human review capacity, and increasing financial stakes. Suppression (0.62): High. Subjects face material barriers to exit (territorial jurisdiction, legal dependency), information barriers (opaque decision criteria), and structural barriers (no alternative decision system). But suppression is not total (some institutions do appeal, some regulators do audit, regulatory mandates are emerging). Theater ratio (0.68): High-moderate. Algorithms are presented as objective, impartial, and efficient — performance theater that legitimizes concentration of authority. Yet institutional administrators often maintain human review processes that reduce theater; EU AI Act mandates push toward lower theater through explainability requirements. The measurement trajectory shows theater increasing as algorithmic complexity outpaced institutional transparency, then declining as regulatory requirements emerged. Current value (0.68) reflects the mixed state: high theater in proprietary systems, lower theater in regulated systems with transparency mandates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The algorithmic subject sees pure extraction (snare) — no coordination benefit, only constraint. The algorithm designer sees pure coordination (rope) — solving a real scaling problem. The institutional administrator sees both (tangled rope) — genuine scaling benefit with embedded extraction cost. The advocacy coalition sees extraction with advocacy channels (tangled rope) — mixed but with agency. The regulatory reformer sees a temporary failure being fixed by standards (scaffold) — sunset mechanism visible. The legacy infrastructure sees degraded but persistent authority (piton) — maintained through theater. The analytical observer sees the gap itself as the diagnosis: the constraint is genuinely tangled at the structural level, not merely perspectival. All seven readings are accurate for their positions; no single type captures the whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions: algorithm designers benefit from the constraint and have arbitrage options (d ≈ 0.10, low), producing negative effective extraction and rope classification. Algorithmic subjects bear costs and are trapped (d ≈ 0.95, maximum), producing high effective extraction and snare classification. Institutional administrators have mixed positions: they benefit from scalability but are constrained by compliance costs (d ≈ 0.55, moderate), producing tangled_rope. The advocacy coalition is pushed by extraction but has advocacy channels and research access (d ≈ 0.60), producing tangled_rope. The reform movement is organized and has regulatory power (d ≈ 0.40, moderate-low), producing scaffold when a sunset mechanism is visible. The analytical observer at civilizational scope sees both coordination (scaling function) and extraction (asymmetric authority) coexisting structurally, producing tangled_rope without perspective collapse. The legacy infrastructure maintains the constraint through arbitrage (institutional prestige, litigation liability management), producing piton through theater ratio gate rather than high extraction chi.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY RESOLUTION: This constraint's mandatrophy is resolved by decomposing into three related stories with different ε values: (1) algorithmic_scaling_coordination (ε=0.22, Rope) — the genuine scaling function, undisputed coordination benefit, low extraction, low suppression; (2) algorithmic_transparency_asymmetry (ε=0.71, Snare) — the information asymmetry and decision opacity embedded in specific algorithmic choices, high extraction, high suppression, high theater; (3) algorithmic_regulation_transition (ε=0.40, Scaffold) — the regulatory reform process (AI Act, transparency mandates, audit requirements) building alternative institutional paths with sunset logic. These three stories are linked via network.affects_constraints: the scaling function enables the transparency asymmetry, and the regulatory movement targets the asymmetry. The unified story presented here (ε=0.58, Tangled Rope) is a perspectival collapse of the family that privileges the analytical position. The agency-specific decomposition would show snare for powerless subjects and rope for designers; the unified story shows tangled_rope for the institutional level. This is correct — the constraint is tangled at the institutional design level, but manifests as pure extraction or pure coordination depending on agent position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_inevitability_vs_contingency,
    'Is algorithmic decision-making an inevitable consequence of institutional scale, or a contingent institutional choice that other scaling mechanisms could replace?',
    'Comparative institutional analysis: examples of large-scale institutions that scale decisions without algorithmic delegation (jury systems, community councils, multi-tier review). Feasibility assessment of alternative scaling architectures (distributed review, human-in-the-loop with transparency, federated decision-making).',
    'If inevitable: the constraint is partly mountain-like (immutable scaling structure). If contingent: the constraint is purely extractive choice (snare). Classification changes from tangled_rope to snare or mountain depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_inevitability_vs_contingency, conceptual, 'Whether algorithmic scaling is inevitable or contingent').

omega_variable(
    algorithmic_bias_structural_vs_intentional,
    'Do algorithmic biases emerge from structural properties of the optimization task (proxy measurement problems, training data imbalance) or from intentional designer choices to optimize for institutional efficiency over fairness?',
    'Algorithmic audits comparing designer stated objectives vs actual optimization constraints; analysis of design alternatives not pursued; comparison across institutions with different design priorities (profit-maximizing vs mission-driven).',
    'If structural: suppression is lower (bias is technical problem with technical solutions). If intentional: suppression is higher (bias is embedded extraction mechanism). Suppression range: 0.35–0.75 depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_structural_vs_intentional, empirical, 'Source of algorithmic bias: structural or intentional').

omega_variable(
    transparency_solution_effectiveness,
    'Does algorithmic transparency (explainability, audit reports, disclosure) actually reduce extraction, or does it create performance theater that obscures opaque decisions behind a façade of accountability?',
    'Tracking outcomes: transparency mandates vs actual decision reversal rates, appeals success rates, demographic outcome changes. Comparison of institutions with transparency requirements vs those without. Analysis of whether transparency reports lead to material algorithm changes or merely compliance documentation.',
    'If effective: regulatory scaffold perspective is real — sunset mechanism works. Theater ratio declines from 0.68 to 0.3–0.4. If theater: transparency mandates become new suppression mechanism (compliance costs, information that doesn''t inform). Theater ratio stays high or increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_solution_effectiveness, empirical, 'Whether transparency mandates reduce extraction or create accountability theater').

omega_variable(
    alternative_scaling_mechanisms_feasibility,
    'What is the actual feasibility-at-scale of alternative institutional decision architectures: human review + algorithmic recommendation (non-binding), federated decision-making, weighted jury systems, staged review with appeals?',
    'Cost-benefit analysis comparing algorithmic delegation to alternatives across different institutional scales. Implementation pilots. Comparison of error rates, appeal rates, demographic outcomes, and administrative overhead.',
    'If alternatives are feasible at reasonable cost: the constraint is contingent (not mountain). If alternatives are infeasible or prohibitively expensive: partial inevitability (constraint may be partly mountain-like). Determines whether scaffold sunset is achievable or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_scaling_mechanisms_feasibility, empirical, 'Feasibility of alternative institutional scaling mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithm_selection_in_institutional_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_inst_tr_t0, algorithm_selection_in_institutional_design, theater_ratio, 0, 0.42).
narrative_ontology:measurement(algo_inst_tr_t3, algorithm_selection_in_institutional_design, theater_ratio, 3, 0.55).
narrative_ontology:measurement(algo_inst_tr_t6, algorithm_selection_in_institutional_design, theater_ratio, 6, 0.68).
narrative_ontology:measurement(algo_inst_tr_t9, algorithm_selection_in_institutional_design, theater_ratio, 9, 0.61).

% Extraction over time
narrative_ontology:measurement(algo_inst_be_t0, algorithm_selection_in_institutional_design, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algo_inst_be_t3, algorithm_selection_in_institutional_design, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(algo_inst_be_t6, algorithm_selection_in_institutional_design, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(algo_inst_be_t9, algorithm_selection_in_institutional_design, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithm_selection_in_institutional_design, resource_allocation).
narrative_ontology:affects_constraint(algorithm_selection_in_institutional_design, algorithmic_transparency_asymmetry).
narrative_ontology:affects_constraint(algorithm_selection_in_institutional_design, algorithmic_scaling_coordination).
narrative_ontology:affects_constraint(algorithm_selection_in_institutional_design, algorithmic_regulation_transition).

% DUAL FORMULATION NOTE:
% The algorithm selection constraint decomposes into a constraint family reflecting different structural levels: scaling coordination (ε≈0.22, rope-type), transparency asymmetry (ε≈0.71, snare-type), and regulatory transition (ε≈0.40, scaffold-type). The unified story (ε=0.58, tangled_rope) represents the institutional-level perspectival collapse. Each family member has its own metrics, omega variables, and temporal measurements. Upstream: scaling coordination enables transparency asymmetry (information asymmetry is only possible when algorithms can scale beyond human review). Downstream: regulatory reform targets transparency asymmetry while preserving scaling coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithm_selection_in_institutional_design, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
