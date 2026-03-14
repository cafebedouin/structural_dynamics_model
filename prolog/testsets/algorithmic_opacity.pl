% ============================================================================
% CONSTRAINT STORY: algorithmic_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_opacity, []).

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
 *   constraint_id: algorithmic_opacity
 *   human_readable: Algorithmic Opacity as Extraction and Coordination
 *   domain: technology/governance/epistemology
 *
 * SUMMARY:
 *   Algorithmic opacity presents a foundational constraint in contemporary
 *   digital governance: the gap between the decisions algorithms make and the
 *   rules that generate those decisions. This gap creates simultaneous
 *   coordination benefits (algorithms scale decision-making beyond human
 *   capacity) and extraction opportunities (opacity enables drift toward
 *   profit optimization while evading accountability). The constraint
 *   exhibits a stable tangled-rope structure across most perspectives, with
 *   boundary cases showing piton degradation (accountability theater) and
 *   snare severity (subjects trapped in opaque decisions with no recourse).
 *   Extractiveness has risen from 0.42 to 0.58 over six years as algorithmic
 *   deployment scaled, while theater ratio has risen from 0.35 to 0.64 as
 *   organizations responded to transparency mandates by adding performative
 *   explainability (audits, fairness reports, documentation) without reducing
 *   underlying opacity. This dual rise (extractiveness + theater) is
 *   diagnostic of a constraint evolving toward piton classification if the
 *   trend continues.
 *
 * KEY AGENTS:
 *   - Algorithmic Subject: Primary victim (powerless/trapped) — individual facing loan denials, job screening, content moderation, or sentencing decisions made by opaque systems with no transparency or meaningful appeal
 *   - Algorithmic Deployer: Primary beneficiary (institutional/arbitrage) — organization (bank, employer, platform, court) capturing efficiency gains and escaping accountability through opacity
 *   - Platform Operator: Institutional beneficiary (institutional/arbitrage) — technology company protecting trade secrets and competitive advantage through opacity requirement
 *   - Regulated Organization: Secondary actor (moderate/constrained) — organizational actor subject to regulation but constrained by technical feasibility and competitive pressure to maintain opacity
 *   - Regulatory Framework: Organized enforcer (organized/constrained) — governments, regulators, legislative bodies imposing explainability mandates and transparency requirements
 *   - Accountability Theater: Institutional degradation (institutional/arbitrage) — auditing, fairness testing, and bias detection rituals that maintain appearance of accountability while preserving opacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as inherently hybrid: coordination genuinely requires some opacity; extraction genuinely requires full opacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_opacity, 0.58).
domain_priors:suppression_score(algorithmic_opacity, 0.68).
domain_priors:theater_ratio(algorithmic_opacity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_opacity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_opacity, tangled_rope).
narrative_ontology:human_readable(algorithmic_opacity, "Algorithmic Opacity as Extraction and Coordination").
narrative_ontology:topic_domain(algorithmic_opacity, "technology/governance/epistemology").

domain_priors:requires_active_enforcement(algorithmic_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_opacity, algorithmic_deployers).
narrative_ontology:constraint_beneficiary(algorithmic_opacity, platform_operators).
narrative_ontology:constraint_victim(algorithmic_opacity, affected_populations).
narrative_ontology:constraint_victim(algorithmic_opacity, field_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC SUBJECT (SNARE) — Individual facing algorithmic decisions (loan denial, job screening, content moderation, criminal sentencing) cannot exit, cannot understand the decision rule, and has no meaningful recourse. Trapped by dependence on services governed by opaque algorithms. Bears full extraction cost with zero transparency or appeal mechanism. Maximum suppression through opacity and structural powerlessness.
constraint_indexing:constraint_classification(algorithmic_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATED ORGANIZATION (TANGLED ROPE) — Organizations deploying algorithms (banks, employers, courts) experience genuine coordination: algorithmic systems scale decision-making, reduce labor costs, and coordinate at pace human review cannot match. But the opacity requirement creates extraction — regulators cannot verify compliance, allowing algorithmic systems to drift toward profit optimization while claiming objectivity. Benefits from coordination alongside asymmetric extraction.
constraint_indexing:constraint_classification(algorithmic_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Technology companies deploying algorithms experience the opacity requirement as pure coordination: trade secret protection, competitive advantage, and IP security all justify opacity. They see it as solving legitimate coordination problems (protecting algorithmic innovations, preventing gaming of ranking systems). Net beneficiary experiencing the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(algorithmic_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (TANGLED ROPE) — Regulators (EU AI Act, algorithmic impact assessments, explainability mandates) see opacity as a problem they must solve through active enforcement. The framework coordinates legitimate deployment of algorithms while enforcing transparency requirements. Constrained by international competition pressure and technical feasibility limits. Both coordination (setting standards) and extraction (compliance burden on smaller actors) present.
constraint_indexing:constraint_classification(algorithmic_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ACCOUNTABILITY THEATER (PITON) — Algorithmic auditing, fairness testing, and bias detection rituals are largely performative. Auditors cannot access full training data or decision rules; tests are designed by the same actors who benefit from opacity; reported fairness metrics often measure proxy variables rather than actual disparate impact. The accountability apparatus persists through institutional requirements (board oversight, compliance reports) despite low verification functionality. Theater ratio elevated by audits that confirm predetermined conclusions.
constraint_indexing:constraint_classification(algorithmic_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, algorithmic opacity contains genuine coordination (scaling decision-making, enabling statistical inference at population level) alongside genuine extraction (preventing accountability, concealing discriminatory drift, suppressing alternative decision rules). Neither component reduces to the other. The constraint is a hybrid with stable equilibrium where both components are necessary to the system's function.
constraint_indexing:constraint_classification(algorithmic_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_opacity, TR),
    TR >= 0.70.

:- end_tests(algorithmic_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Algorithmic opacity enables organizations to avoid accountability for discriminatory drift, unequal treatment, and outcome disparities. The extraction is not maximal because some opacity serves legitimate coordination (protecting algorithmic innovation, preventing gaming). The value reflects the balance: substantial asymmetric benefit to deployers alongside genuine cost to subjects. Suppression (0.68): High. Subjects cannot understand decision rules, cannot contest decisions with evidence of how the algorithm works, cannot migrate to alternative systems, and face structural barriers to collective action (atomization of individual grievances). Opacity itself is the suppression mechanism. Theater ratio (0.64): Moderate-high. Algorithmic fairness audits, bias detection, and explainability requirements are largely performative: auditors cannot access training data or full decision rules; fairness metrics measure proxies rather than actual impact; audits are conducted by actors with incentive to find 'acceptable' levels of bias; reported improvements do not correlate with actual changes in decision outcomes for affected populations. The theater has increased as regulatory pressure mounted — organizations added audit infrastructure without reducing opacity.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals that algorithmic opacity is not a simple extraction mechanism but a structural feature with genuine dual function. Subjects see pure snare because they are atomized and powerless — they cannot access the coordination benefits of scale, only the extraction costs of opacity. Deployers see pure rope because they are the beneficiaries of scale and IP protection — they access the full coordination function. Regulators see tangled rope because they are trying to enforce transparency (fighting extraction) while allowing algorithmic deployment to continue (preserving coordination). The piton classification shows that accountability theater has become a major component of the constraint's function — organizations maintain opacity while performing accountability, and regulators accept performance as compliance. This is diagnostic: if theater ratio continues rising above 0.70, the constraint migrates from tangled rope toward piton, indicating that the accountability apparatus has decoupled from actual oversight.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithmic subjects (powerless/trapped) experience maximum directionality toward extraction (d ≈ 0.95, f(d) ≈ 1.4): they are trapped, dependent, and benefit from the constraint zero. Algorithmic deployers (institutional/arbitrage) experience minimum directionality (d ≈ 0.05, f(d) ≈ -0.12): they benefit from opacity, have exit options (can locate to permissive jurisdictions), and experience the constraint as enabling. Regulated organizations (moderate/constrained) experience moderate directionality (d ≈ 0.65, f(d) ≈ 1.0): they face real constraints from transparency mandates but gain coordination benefits. The regulatory framework itself (organized/constrained) experiences moderate directionality (d ≈ 0.55, f(d) ≈ 0.75): it enforces constraints but is itself constrained by technical feasibility and international competition. The analytical observer (analytical/analytical) experiences baseline directionality (d ≈ 0.73, f(d) ≈ 1.15): they see the structure from outside all positions but are not exempt from the constraint's logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing opacity into two load-bearing components: (1) legitimate coordination (scaling decision-making, statistical power, consistency) requires some algorithmic complexity and inherent uninterpretability, and (2) extraction (evading accountability, concealing discriminatory drift) requires full opacity beyond what coordination needs. The mandatrophy question — 'Is this coordination or extraction?' — has a true answer: BOTH. The constraint cannot be classified as pure rope (coordination) because extraction is real and substantial. It cannot be classified as pure snare (extraction) because coordination benefits are real and cannot be achieved at scale through alternative means. The tangled rope classification is correct and stable. The rising theater ratio (0.35→0.64) reveals that regulatory response has added performance without reducing extraction, which is the signature of mandatrophy escalation. The piton component (accountability theater) is growing faster than genuine transparency. If this trend continues, the constraint becomes piton-dominant, indicating full regulatory capture: the appearance of accountability with preservation of opacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explainability_sufficiency,
    'Does algorithmic explainability (interpretability, fairness testing, documentation) actually enable meaningful accountability, or is it theater that simulates accountability while preserving opacity?',
    'Comparison of audit findings vs downstream outcome changes: Do explainability disclosures correlate with actual algorithm modification or regulatory enforcement? Do they change decision outcomes for affected parties?',
    'If explainability enables accountability: piton component reduces, extraction drops, constraint moves toward rope/scaffold. If it is theater: suppression increases, theater ratio rises further, constraint remains snare/tangled rope with stable opacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(explainability_sufficiency, empirical, 'Whether explainability disclosure enables actual accountability').

omega_variable(
    technical_tractability_bound,
    'Are there inherent technical limits to explainability — fundamental tradeoffs between model performance and interpretability — that make full transparency mathematically impossible?',
    'Scaling studies of deep learning model performance vs interpretability; comparison of black-box performance to interpretable-by-design alternatives; domain-specific analysis of complexity ceiling',
    'If technical bounds are real and severe: some opacity is unavoidable (mountain component emerges). If bounds are loose or overcome by architecture choice: opacity is policy choice, not technical necessity. Extraction component increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_tractability_bound, empirical, 'Technical tractability limits on algorithmic explainability').

omega_variable(
    regulatory_arbitrage,
    'Are organizations genuinely constrained by algorithmic opacity regulations, or do they maintain opacity by regulatory arbitrage — deploying opaque systems in loosely regulated jurisdictions while using explainable-by-design in strict ones?',
    'Cross-jurisdiction comparison: algorithmic transparency mandates vs actual opacity in deployment. Tracking of deployment location decisions relative to regulatory environment. Analysis of whether strict regulation changes algorithm design or just deployment geography.',
    'If arbitrage dominates: regulation is theater (piton), extraction persists globally, suppression of accountability continues. If regulations bind globally: constraint moves toward enforcement-based tangled rope, suppression decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage, empirical, 'Whether regulatory arbitrage enables opacity despite formal mandates').

omega_variable(
    coordination_vs_extraction_decomposition,
    'Can the genuine coordination function of algorithmic systems (scaling, efficiency, consistency) be separated from the extraction function (opacity, suppression of accountability), or are they structurally inseparable?',
    'Analysis of algorithmic deployment cases with mandatory transparency: Do transparent algorithms lose coordination benefits? Can organizations achieve scale and efficiency without opacity? Historical comparison of pre-algorithmic decision systems vs transparent algorithmic systems vs opaque algorithmic systems on coordination metrics.',
    'If separable: decompose into two constraints (algorithmic_coordination and algorithmic_opacity_extraction). If inseparable: opacity is a load-bearing structural component and cannot be removed without degrading coordination function. Confirms tangled rope across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, empirical, 'Whether coordination and extraction functions are separable in algorithmic systems').

omega_variable(
    identity_lock_in_deployers,
    'Do organizations deploying opaque algorithms become identity-locked to opacity as a core operational identity, such that transparency would require fundamental reorganization of their self-conception as technology leaders?',
    'Organizational response patterns to transparency mandates: Do organizations shift to transparent-by-design, or do they maintain opacity while adding performative explainability? Qualitative analysis of organizational rhetoric about algorithm design philosophy and competitive advantage.',
    'If identity-locked: deployers experience transparency mandates as identity-threatening; they will migrate to opaque-friendly jurisdictions or add theater rather than genuine transparency. Explains persistent opacity despite regulation. If not: deployers can shift to transparent models without identity crisis; regulatory constraint is straightforward (high suppression but not identity-level).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_deployers, conceptual, 'Whether algorithmic opacity is embedded in organizational identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_opacity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algop_tr_t0, algorithmic_opacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algop_tr_t3, algorithmic_opacity, theater_ratio, 3, 0.48).
narrative_ontology:measurement(algop_tr_t6, algorithmic_opacity, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(algop_be_t0, algorithmic_opacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(algop_be_t3, algorithmic_opacity, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(algop_be_t6, algorithmic_opacity, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_opacity, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_opacity, algorithmic_bias_distribution).
narrative_ontology:affects_constraint(algorithmic_opacity, platform_governance_opacity).
narrative_ontology:affects_constraint(algorithmic_opacity, regulatory_capture_ai_deployment).

% DUAL FORMULATION NOTE:
% Algorithmic opacity decomposes into two structurally distinct constraints when measured by different observables: (1) algorithmic_explainability_technical (ε=0.08, mountain-class) — the technical limit on interpretability of high-dimensional statistical models, and (2) algorithmic_opacity_governance (ε=0.58, tangled rope, this story) — the institutional choice to maintain opacity beyond technical necessity. The two are linked: genuine technical limits provide cover for institutional opacity choices. Regulatory debate conflates them, treating governance opacity as if it were technical necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_opacity, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
